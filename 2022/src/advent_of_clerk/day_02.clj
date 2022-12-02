;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [clojure.string :as str]))

(def input (slurp "resources/day02.txt"))

(def shape-score {"X" 1
                  "Y" 2
                  "Z" 3
                  ["A" "X"] 3
                  ["B" "X"] 1
                  ["C" "X"] 2
                  ["A" "Y"] 1
                  ["B" "Y"] 2
                  ["C" "Y"] 3
                  ["A" "Z"] 2
                  ["B" "Z"] 3
                  ["C" "Z"] 1})

(def outcome-score {["A" "X"] 3
                    ["B" "X"] 0
                    ["C" "X"] 6
                    ["A" "Y"] 6
                    ["B" "Y"] 3
                    ["C" "Y"] 0
                    ["A" "Z"] 0
                    ["B" "Z"] 6
                    ["C" "Z"] 3
                    "X" 0
                    "Y" 3
                    "Z" 6})

(defn parse-input [input]
  (mapv #(re-seq #"\w" %) (str/split-lines input)))

(defn result [f]
  (->> input parse-input (reduce f 0)))

(defn part1 [sum [_ yours :as game]]
  (+ sum (shape-score yours) (outcome-score game)))

(result part1)

(defn part2 [sum [_ result :as game]]
  (+ sum (shape-score game) (outcome-score result)))

(result part2)
