(ns advent-2020.day01
  (:require [clojure.string :as str]))

(def exercise-input [1721 979 366 299 675 1456])

(def input (->> (slurp "resources/day01.txt")
               str/split-lines
               (mapv parse-long)))

(defn part1 [input]
  (->> input
       (mapcat (fn [a] (map (fn [b] [a b]) input)))
       (filter (fn [[a b]] (= (+ a b) 2020)))
       first
       (apply *)))

(defn part2 [input]
  (->> input
       (mapcat (fn [a] (mapcat (fn [b] (map (fn [c] [a b c]) input)) input)))
       (filter (fn [[a b c]] (= (+ a b c) 2020)))
       first
       (apply *)))

(comment
  (part1 exercise-input)
  (part2 exercise-input)
  (part1 input)
  (part2 input))
