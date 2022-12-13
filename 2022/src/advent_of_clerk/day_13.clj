;; # 🎄 Advent of Clerk: Day 13
(ns advent-of-clerk.day-13
  (:require [clojure.string :as str]))

(def input (slurp "resources/day13.txt"))

(defn parse-input [input]
  (mapv (fn [pair] (mapv read-string (str/split-lines pair))) (str/split input #"\n\n")))

(defn- compare-pair [[left & rest-left] [right & rest-right]]
  (cond
    (and (int? left) (int? right))
    (cond
      (< left right) -1
      (> left right) 1
      :else (compare-pair rest-left rest-right))

    (and (coll? left) (coll? right))
    (let [res (compare-pair left right)]
      (if (= res :continue) (compare-pair rest-left rest-right) res))

    (and (int? left) (coll? right))
    (let [res (compare-pair [left] right)]
      (if (= res :continue) (compare-pair rest-left rest-right) res))

    (and (coll? left) (int? right))
    (let [res (compare-pair left [right])]
      (if (= res :continue) (compare-pair rest-left rest-right) res))

    (and (nil? left) (nil? right)) :continue

    (nil? left) -1

    (nil? right) 1))

(defn part1 [input]
  (->> input
       parse-input
       (keep-indexed (fn [idx pair] (when (= -1 (apply compare-pair pair)) (inc idx))))
       (reduce +)))

(part1 input)

(defn part2 [input]
  (let [dividers [[[2]] [[6]]]
        sorted-packets (->> dividers
                            (apply conj (mapcat identity (parse-input input)))
                            (sort compare-pair))]
    (->> dividers
         (map (fn [divider] (inc (.indexOf sorted-packets divider))))
         (apply *))))

(part2 input)
