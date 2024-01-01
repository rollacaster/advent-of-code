(ns advent-of-code.day07
  (:require [clojure.string :as str]))

(def input (slurp "resources/day07.txt"))

(defn rank [hand]
  (let [duplicates (vals (frequencies hand))]
    (cond
      ((set duplicates) 5) 6
      ((set duplicates) 4) 5
      (and ((set duplicates) 3)
           ((set duplicates) 2)) 4
      ((set duplicates) 3) 3
      (= (count (filter #(= % 2) duplicates)) 2) 2
      ((set duplicates) 2) 1
      :else 0)))

(defn- sort-hands [rank strength [hand1] [hand2]]
  (let [rank1 (rank hand1)
        rank2 (rank hand2)]
    (cond
      (> rank1 rank2) 1
      (< rank1 rank2) -1
      :else (some
             (fn [[a b]]
               (when (not= (strength a) (strength b))
                 (if (> (strength a) (strength b))
                   1 -1)))
             (mapv vector hand1 hand2)))))

(defn run [sort-fn input]
  (->> input
       str/split-lines
       (mapv (fn [line] (update (str/split line #" ") 1 parse-long)))
       (sort sort-fn)
       (map-indexed (fn [i [_ bid]] (* (inc i ) bid)))
       (reduce +)))

(defn part1 [input]
  (run (partial sort-hands rank {\A, 12 \K, 11 \Q 10, \J 9, \T 8, \9 7, \8 6, \7 5, \6 4, \5 3, \4 2, \3 1, \2 0})
    input))

(defn rank2 [hand]
  (let [hand-frequencies (frequencies hand)
        duplicates (vals hand-frequencies)]
    (cond
      ((set duplicates) 5) 6
      (and ((set duplicates) 4) (hand-frequencies \J)) 6
      ((set duplicates) 4) 5
      (and ((set duplicates) 3) ((set duplicates) 2) (hand-frequencies \J)) 6
      (and ((set duplicates) 3) ((set duplicates) 2)) 4
      (and ((set duplicates) 3) (hand-frequencies \J)) 5
      ((set duplicates) 3) 3
      (and (= (count (filter #(= % 2) duplicates)) 2) (= (hand-frequencies \J) 2)) 5
      (and (= (count (filter #(= % 2) duplicates)) 2) (= (hand-frequencies \J) 1)) 4
      (= (count (filter #(= % 2) duplicates)) 2) 2
      (and ((set duplicates) 2) (hand-frequencies \J)) 3
      ((set duplicates) 2) 1
      (hand-frequencies \J) 1
      :else 0)))


(defn part2 [input]
  (run (partial sort-hands rank2
                {\A, 12 \K, 11 \Q 10, \T 9, \9 8, \8 7, \7 6, \6 5, \5 4, \4 3, \3 2, \2 1 \J 0})
    input))

(comment
  (part1 input)
  ;; => 249390788
  (part2 input)
  ;; => 248750248
)
