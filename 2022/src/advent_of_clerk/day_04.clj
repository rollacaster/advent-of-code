;; # 🎄 Advent of Clerk: Day 4
(ns advent-of-clerk.day-04
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "resources/day04.txt"))

(defn ->range [line]
  (->> (str/split line #",")
       (mapcat #(str/split % #"-"))
       (map parse-long)
       (partition 2)
       (map (fn [[a b]] (set (range a (inc b)))))))

(defn part1 [input]
  (->> input
       str/split-lines
       (map ->range)
       (filter (fn [[a b]] (or (set/subset? a b) (set/subset? b a))))
       count))

(defn part2 [input]
  (->> input
       str/split-lines
       (map ->range)
       (filter (fn [[a b]] (seq (set/intersection a b))))
       count))

(part1 input)
(part2 input)
