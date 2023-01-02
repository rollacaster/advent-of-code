;; # 🎄 Advent of Clerk: Day 16
(ns advent-of-clerk.day-16
  (:require [clojure.string :as str]))

(def input (slurp "resources/day16.txt"))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [line]
                 (let [[_ valve-and-rate tunnels] (str/split line #"[Vv]alves?\s")]
                   [(subs valve-and-rate 0 2)
                    {:rate (parse-long (re-find #"\d+" valve-and-rate))
                     :tunnels (set (str/split tunnels #", "))}])))
       (into {})))

(defn nodes [scan]
  (filter (fn [[from]] (or (= from "AA") (> (:rate (get scan from)) 0))) scan))

(defn- replace-zeros [scan from tunnels cost]
  (mapcat
   (fn [to]
     (if (or (pos? (:rate (get scan to))) (= to "AA"))
       [[to (inc cost)]]
       (replace-zeros scan to (disj (:tunnels (get scan to)) from) (inc cost))))
   tunnels))

(defn- edges [scan valves]
  (->> valves
       (map (fn [[from {:keys [tunnels]}]]
              (->> (replace-zeros scan from tunnels 0)
                   (map (fn [[to cost]] [[from to] cost]))
                   (into {}))))
       (apply merge)))

(defn floyd-warshall [nodes edges]
  (let [edges (transient edges)
        cost (fn [[from to]] (if (= from to) 0 (or (edges [from to]) ##Inf)))]
    (doseq [k nodes i nodes j nodes]
      (if (< (cost [i j]) (+ (cost [i k]) (cost [k j])))
        edges
        (assoc! edges [i j] (+ (cost [i k]) (cost [k j])))))
    (persistent! edges)))

(defn world [input]
  (let [scan (parse-input input)
        nodes (nodes scan)
        edges (floyd-warshall (keys nodes) (edges scan nodes))]
    (->> nodes
         (map
          (fn [[from desc]]
            [from
             (assoc desc :tunnels
                    (->> edges
                         (filter (fn [[[f t]]] (and (= f from) (not= t f) (not= t "AA"))))
                         (map (fn [[[_ t] cost]] [t cost]))
                         (into {})))]))
         (into {}))))

(defn a-star-search
  [initial final? generate-moves heuristic]
  (let [to-visit (java.util.PriorityQueue. 10000 (comparator (fn [[a] [b]] (> a b))))]
    (loop [[_ state] [(heuristic initial) initial]
           visited #{}]
      (when (not (visited [(:position state) (:pressure state)]))
        (let [moves (generate-moves state)]
          (doseq [nxt-state moves]
            (when (not (visited [(:position state) (:pressure state)]))
              (.add to-visit [(heuristic nxt-state) nxt-state])))))
      (cond (final? state) state
            (empty? to-visit) state
            :else (recur (.poll to-visit)
                         (conj visited [(:position state)
                                        (:pressure state)]))))))

(let [world (world input)]
  (a-star-search {:position ["AA" "AA"]
                  :pressure 0
                  :open-valves #{}
                  :minutes [26 26]
                  :path []}
                 (fn [{:keys [open-valves minutes]}]
                   (or (= (count open-valves) (count (->> (vals world) (map :rate) (filter pos?))))
                       (every? #(= 0 %) minutes)))
                 (fn [{:keys [position minutes open-valves pressure path]}]
                   (let [[m1 m2] minutes
                         [p1 p2] position]
                     (if (not= m1 0)
                       (keep
                        (fn [[to cost]]
                          (when (not (open-valves to))
                            {:position [to p2]
                             :pressure (+ pressure (* (:rate (get world to)) (- m1 (inc cost))))
                             :minutes  [(max 0 (- m1 (inc cost))) m2]
                             :open-valves (conj open-valves to)
                             :path (conj path [[:you to m1]])}))
                        (:tunnels (get world p1)))
                       (keep
                        (fn [[to cost]]
                          (when (not (open-valves to))
                            {:position [p1 to]
                             :pressure (+ pressure (* (:rate (get world to)) (- m2 (inc cost))))
                             :minutes  [m1 (max 0 (- m2 (inc cost)))]
                             :open-valves (conj open-valves to)
                             :path (conj path [[:ele to m2]])}))
                        (:tunnels (get world p2))))))
                 (fn [{:keys [pressure open-valves minutes]}]
                   (let [rates (->> (vals world) (map :rate) (filter pos?))
                         avg-rate (/ (reduce + rates) (count rates))]
                     (+ pressure (* avg-rate (- (count rates) (count open-valves)) (apply max minutes)))))))
