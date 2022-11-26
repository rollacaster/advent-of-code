(ns advent-2020.day20
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data]))
(def input (slurp "resources/day20.txt"))

(defn parse-input [input]
  (apply
   hash-map
   (mapcat
    (fn [tile-str]
      [(parse-long (re-find #"\d+" tile-str))
       (str/split (second (str/split tile-str #"\d+:\n")) #"\n")])
    (str/split input #"\R\R"))))

(defn rotate [tile] (apply mapv (fn [& line] (apply str (reverse line))) tile))
(defn flip-y [tile] (vec (reverse tile)))
(defn flip-x [tile] (mapv (fn [x] (apply str (reverse x))) tile))

(def ops (take 30 (cycle
                  [identity
                   rotate
                   (comp rotate rotate)
                   (comp rotate rotate rotate)
                   flip-y
                   (comp rotate flip-y)
                   (comp rotate rotate flip-y)
                   (comp rotate rotate rotate flip-y)
                   flip-x
                   (comp rotate flip-x)
                   (comp rotate rotate flip-x)
                   (comp rotate rotate rotate flip-x)])))

(defn tile-edges [tile]
  [(first tile)
   (map first tile)
   (map last tile)
   (last tile)])

(def dir [:top :left :right :bottom])

(defn tile-content [tile]
  (->> tile
       (drop 1)
       (drop-last 1)
       (mapv (fn [line] (->> line (drop 1) (drop-last 1) (apply str))))))

(defn find-connections [tile-id tile-id->tile op]
  (keep-indexed
   (fn [i edge]
     (when-let [neighbour-id
                (some
                 (fn [[neighbour-id neighbour-tile]]
                   (->> neighbour-tile
                        op
                        tile-edges
                        (map-indexed vector)
                        (some (fn [[neighbour-i neighbour-edge]] (when (and (= neighbour-edge edge) (not= neighbour-i i)) neighbour-id)))))
                 (dissoc tile-id->tile tile-id))]
       [(nth dir i) tile-id neighbour-id]))
   (tile-edges (tile-id->tile tile-id))))

(def part1 (memoize (fn[input]
                      (let [tile-id->tile (-> input parse-input)]
                        (apply hash-map
                               (mapcat
                                (fn [[tile-id]]
                                  [tile-id (->> ops
                                                (take 8)
                                                (mapcat (fn [op] (map last (find-connections tile-id tile-id->tile op))))
                                                set
                                                count)])
                                tile-id->tile))))))

(defn find-start [tile-id->tile]
  (some
   (fn [[tile-id]] (when (seq (find-connections tile-id tile-id->tile identity))
                         tile-id))
   tile-id->tile))

(defn v-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn neighbour-count [result pos]
  (->> [[0 -1] [0 1] [-1 0] [1 0]]
       (keep (fn [dir] (get (set/map-invert result) (v-add pos dir))))
       count))

(defn- build-result [result connections]
  (reduce
   (fn [res [dir from to]]
     (assoc res to (v-add
                    (get res from)
                    (case dir
                      :top [0 -1]
                      :bottom [0 1]
                      :left [-1 0]
                      :right [1 0]))))
   result
   connections))

(defn build-puzzle [res tile-id->tile]
  (->> res
       set/map-invert
       (sort-by first (fn [[x1 y1] [x2 y2]] (cond
                                             (< x1 x2) true
                                             (and(= x1 x2) (< y1 y2)) true
                                             :else false)))
       (group-by (fn [[[_ y]]] y))
       (sort-by first)
       (mapcat (fn [[_ tiles]]
                 (apply map
                        (fn [& lines] (apply str lines))
                        (map (fn [[_ tile-id]] (-> tile-id tile-id->tile tile-content)) tiles))))))

(def sea-monster (map
                  re-pattern
                  (str/split (str/replace "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   " " " ".")
                             #"\n")))

(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
      (when (. m find)
        (cons {:start (. m start) :end (. m end) :group (. m group)}
          (lazy-seq (step))))))))

(defn- count-monsters [result tile-id->tile]
  (let [rotated-puzzles (map (fn [op] (-> result (build-puzzle tile-id->tile) op)) ops)
        puzzle-count (count (filter #(= % \#) (apply concat (first rotated-puzzles))))]
    (->> rotated-puzzles
         (map
          (fn [puzzle]
            (count (mapcat
                    (fn [line-number ]
                      (keep
                       (fn [{:keys [start end]}]
                         (and (re-find (first sea-monster) (subs (nth puzzle (dec line-number)) start end))
                              (re-find (nth sea-monster 2) (subs (nth puzzle (inc line-number)) start end))))
                       (re-seq-pos
                        (second sea-monster)
                        (nth puzzle line-number))))
                    (range 1 (count puzzle))))))
         (filter (fn [count] (> count 0)))
         first
         (* 15)
         (- puzzle-count))))

(defn part2 [input]
  (let [tile-id->tile (-> input parse-input)
        neighbours (part1 input)
        start-tile-id (find-start tile-id->tile)
        [result tile-id->tile]
        (loop [[tile-id & other-tile-ids] [start-tile-id]
               already-seen #{}
               result {start-tile-id [0 0]}
               op-index 0
               tile-id->tile tile-id->tile]
          (if (nil? tile-id)
            (if (= (count already-seen) (count tile-id->tile))
              [result tile-id->tile]
              (let [[q already-seen] (->> result
                                          keys
                                          ((juxt filter remove) #(not= (neighbour-count result (result %)) (neighbours %)))
                                          (map set))]
                (recur
                 q
                 already-seen
                 result
                 (inc op-index)
                 tile-id->tile)))
            (let [connections (find-connections tile-id tile-id->tile (nth ops op-index))
                  new-tile-ids (map (fn [[_ _ to]] to) connections)]
              (recur
               (set/difference (set (concat other-tile-ids new-tile-ids)) already-seen)
               (conj already-seen tile-id)
               (build-result result connections)
               op-index
               (reduce
                (fn [tile-id->tile [_ _ to]]
                  (if (get result to)
                    tile-id->tile
                    (update tile-id->tile to (nth ops op-index))))
                tile-id->tile
                connections)))))]
    (count-monsters result tile-id->tile)))

(part2 input)
;;2246
