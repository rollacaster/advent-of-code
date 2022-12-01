;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require [clojure.string :as str]))

(def input (slurp "resources/day01.txt"))

(defn part1 [input]
  (->> (str/split input #"\n\n")
       (map #(->> % (re-seq #"\d*") (keep parse-long) (reduce +)))
       (apply max)))

(part1 input)

(defn part2 [input]
  (->> (str/split input #"\n\n")
       (map #(->> % (re-seq #"\d*") (keep parse-long) (reduce +)))
       (sort >)
       (take 3)
       (reduce +)))

(part2 input)
