;; # 🎄 Advent of Clerk: Day 15
(ns advent-of-clerk.day-15
  (:require [clojure.string :as str]))

(def input (slurp "resources/day15.txt"))

(defn parse-input [input]
  (mapv (fn [line] (->> line
                       (re-seq #"-?\d+")
                       (map parse-long)
                       (partition 2)
                       (map vec)
                       vec))
        (str/split-lines input)))

(defn- find-empty-positions [empty-positions [[sx sy] [bx by]]]
  (let [beacon-dist (+ (Math/abs (- sx bx)) (Math/abs (- sy by)))]
    (conj
     empty-positions
     [[(- sx beacon-dist) (+ sx beacon-dist)]
      [(- sy beacon-dist) (+ sy beacon-dist)]])))

(defn empty-ranges [sensors row]
  (->> sensors
       (reduce find-empty-positions #{})
       (filter (fn [[_ [min-y max-y]]] (<= min-y row max-y)))
       (map (fn [[[min-x max-x] [min-y max-y]]]
              (let [y (/ (+ max-y min-y) 2)
                    center-distance (Math/abs (- y row))]
                [(+ min-x center-distance) (- max-x center-distance)])))))

(defn part1 [input row]
  (let [sensors (parse-input input)
        xs (->> (empty-ranges sensors row)
                (mapcat (fn [[from to]] (range from (inc to))))
                set
                sort
                count)]
    (dec xs)))

(part1 input 2000000)

(defn- unify-ranges [xs]
  (loop [[[from to] & ranges] (set xs)
         result []]
    (if (nil? ranges)
      (conj result [from to])
      (if-let [[f t] (some (fn [[f t]] (when (or (<= (dec f) from (inc t)) (<= (dec f) to (inc t))
                                                (and (<= from f) (>= to t))
                                                (and (<= f from) (>= t to)))
                                        [f t])) ranges)]
        (recur (-> (set ranges) (disj [f t]) (conj [(min f from) (max t to)])) result)
        (recur ranges (conj result [from to]))))))


(defn- scanned-positions-in-row [row sensors max-value]
  (->> (empty-ranges sensors row)
       (map (fn [[min-x max-x]]
              [(max 0 min-x) (min max-value max-x)]))
       unify-ranges))

(defn part2 [input max-value]
  (let [sensors (parse-input input)
        [y x] (some
               (fn [row]
                 (when (> (count (scanned-positions-in-row row sensors max-value)) 1)
                   [row (scanned-positions-in-row row sensors max-value)]))
               (range 3200000 max-value))]
    (+ (* 4000000 (inc (last (first (sort-by first x))))) y)))

(part2 input 4000000)
