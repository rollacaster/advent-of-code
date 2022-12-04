;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.day-03
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "resources/day03.txt"))

(defn letter->int [letter-int]
  (cond
    (<= 65 letter-int 90) (- letter-int 38)
    (<= 97 letter-int 122) (- letter-int 96)))

(defn- bag [bag-line]
  (->> bag-line
       (map (comp letter->int int))
       (split-at (/ (count bag-line) 2))
       (map set)
       (apply set/intersection)
       first))

(defn part1 [input]
  (->> input
       str/split-lines
       (mapv bag)
       (reduce +)))

(defn- bag-group [bag-lines]
  (->> bag-lines
       (map #(set (map (comp letter->int int) %)))
       (apply set/intersection)
       first))

(defn part2 [input]
  (->> input
       str/split-lines
       (partition 3)
       (map bag-group)
       (reduce +)))

(part1 input)

(part2 input)
