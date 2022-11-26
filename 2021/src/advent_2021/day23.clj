(ns advent-2021.day23
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(def input2 "#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########")

(def input3 "#############
#...........#
###A#D#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #B#C#A#C#
  #########")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapcat #(re-seq #"\w+" %))
       (map keyword)
       (partition 4)
       (apply map vector)))

(def costs {:A 1 :B 10 :C 100 :D 1000})

(defn room-nodes [[room-type :as node]]
  (cond (every? #(or (= room-type %) (= :empty %)) node) nil
        :else (first (drop-while #(= (second %) :empty) (rest (map-indexed vector node))))))

(defn nodes [state]
  (reduce
   (fn [nodes [pos node]]
     (cond (= node :empty) nodes
           (and (vector? node) (nil? (room-nodes node))) nodes
           :else
           (conj
            nodes
            (if (vector? node)
              (let [[room-pos amph] (room-nodes node)]
                {:amph amph
                 :pos [pos room-pos]})
              {:amph node
               :pos [pos]}))))
   []
   (map-indexed vector state)))

(defn nonblocked-spaces [{:keys [pos]} state]
  (let [pos (first pos)
        all-pos (map-indexed vector state)
        [above below] [(take pos all-pos) (drop (inc pos) all-pos)]]
    (concat (take-while (fn [[_ curr]] (or (= curr :empty) (vector? curr))) (reverse above))
            (take-while (fn [[_ curr]] (or (= curr :empty) (vector? curr))) below))))

(defn room-edges [{:keys [amph]}spaces]
  (->> spaces
       (filter (fn [[_ [room-type :as room]]]
                 (and (= room-type amph)
                      (every? #(or (= % :empty) (= % amph)) room))))))

(defn room-edges-cost [{:keys [pos amph]} room-edges]
  (->> room-edges
       (map (fn [[edge-pos room-pos]]
              (let [room-pos
                    (if (every? #(= :empty %) (drop 1 room-pos))
                      (count (drop 1 room-pos))
                      (->> room-pos
                           (map-indexed vector)
                           (drop 1)
                           (filter #(not= :empty (second %)))
                           ffirst
                           dec))]
                [[edge-pos room-pos]
                 (*
                  (costs amph)
                  (+ (Math/abs (- (first pos) edge-pos))
                     room-pos
                     (or (second pos) 0)))])))))

(defn hallway-egdes-costs [{:keys [pos amph]} spaces]
  (->> spaces
       (map (fn [[edge-pos]]
              [[edge-pos]
               (*
                (costs amph)
                (+ (Math/abs (- (first pos) edge-pos))
                   (second pos)))]))))

(defn edges [{:keys [pos] :as node} state]
  (let [nonblocked-spaces (nonblocked-spaces node state)
        grouped-spaces (group-by (fn [[_ curr]] (vector? curr)) nonblocked-spaces)
        room-spaces (grouped-spaces true)
        hallway-spaces (grouped-spaces false)
        room-edges (room-edges node room-spaces)
        in-hallway? (= (count pos) 1)]
    (cond (and (empty? room-edges) in-hallway?) ()
          in-hallway? (room-edges-cost node room-edges)
          (not-empty room-edges) (room-edges-cost node room-edges)
          :else (hallway-egdes-costs node hallway-spaces))))

(defn finish? [state]
  (->> state
       (filter #(vector? %))
       (every? #(= (count (distinct %)) 1))))

(def cur-min (atom ##Inf))
(def visited (atom #{}))

(def possible-moves
  (memoize (fn [state]
             (for [node (nodes state)
                   edge (sort-by second (edges node state))]
               [node edge]))))

(defn step [state {:keys [amph pos]} [next]]
  (let [next
        (-> state
            (assoc-in pos :empty)
            (assoc-in next amph))]
    next))

(defn move [curr-cost path state]
  (cond (:finish state)
        (do
          (when (< curr-cost @cur-min)
            (reset! cur-min curr-cost))
          [(-> state (assoc :costs curr-cost))])
        :else
        (let [possible-moves (possible-moves state)]
          (when (> (count possible-moves) 0)
            (for [[node edge] possible-moves
                  :let [costs (+ curr-cost (second edge))
                        path (conj path [node edge])]]
              (when-not (@visited path)
                (swap! visited conj path)
                (if (> costs @cur-min)
                  nil
                  {:moves (move costs
                                path
                                (if (finish? (step2 state node edge))
                                  {:finish true}
                                  (step2 state node edge)))})))))))

(defn play [input]
  {:moves
   (move
    0
    #{}
    (let [[a b c d] (parse-input input)]
      [:empty :empty
       (vec (cons :A a)) :empty
       (vec (cons :B b)) :empty
       (vec (cons :C c)) :empty
       (vec (cons :D d))
       :empty :empty]))})

(defn part1 [input]
  (reset! cur-min ##Inf)
  (reset! visited #{})
  (time
   (->> (tree-seq :moves :moves (play input))
        (filter (fn [node] (= (:finish node) true)))
        (sort-by :costs)
        first)))

(defn play2 [input]
  {:moves
   (move
    0
    #{}
    (let [[a b c d] (parse-input input)]
      [:empty :empty
       (vec (cons :A a)) :empty
       (vec (cons :B b)) :empty
       (vec (cons :C c)) :empty
       (vec (cons :D d))
       :empty :empty]))})

(defn part2 [input2]
  (loop [games
         #{[(let [[a b c d] (parse-input input2)]
              [:empty :empty
               (vec (cons :A a)) :empty
               (vec (cons :B b)) :empty
               (vec (cons :C c)) :empty
               (vec (cons :D d))
               :empty :empty])
            0]}
         turn 0]
    (prn (str "Games: " (count games) " Turn: " turn))
    (cond
      ;; (= turn 5) (def gameies games)
      (some finish? (map first games))
      (->> games
           (filter (comp finish? first))
           (sort-by second)
           first
           second)
      :else
      (recur
       (set
        (for [[state cost] games
              node (nodes state)
              edge (edges node state)]
          [(step2 state node edge)
           (+ cost (second edge))]))
       (inc turn)))))
(comment
  (part1 input)
  (part1 input3) ;; 44618
  (part2 input) ;; 12521
  (part2 (slurp (io/resource "day23.txt"))) ;; 12240
  (part2 input2) ;; 44169
  (part2 input3);; 49220 wrong ¯\_(ツ)_/¯
)





