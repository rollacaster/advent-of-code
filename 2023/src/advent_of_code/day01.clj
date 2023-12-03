(ns advent-of-code.day01
  (:require [clojure.string :as str]))

(def input (slurp "resources/day01.txt"))

(def name->digit
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn part1 [line]
  (re-seq #"\d" line))

(defn part2 [line]
  (->> line
       (re-seq (re-pattern (str (str/join "|" (keys name->digit)) "|\\d")))
       (map #(or (name->digit %) %))))

(defn run [f]
  (->> input
       str/split-lines
       (map (comp parse-long #(apply str %) (juxt first last) f))
       (reduce +)))

(run part1)
;; => 54561

(run part2)
;; => 54076
