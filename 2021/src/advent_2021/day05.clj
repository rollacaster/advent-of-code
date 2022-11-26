(ns advent-2021.day05
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]))

(defn parse-input [input]
  (->> input
       (re-seq #"(\d+),(\d+) -> (\d+),(\d+)")
       (map rest)
       (map #(->> % (map parse-long) (partition 2)))))

(defn vertical-or-horizontal? [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2)))

(defn range-2 [[start end]]
  (range start
         ((if (> start end) dec inc) end)
         (if (> start end) -1 1)))

(defn combine-coords [line-segment coords]
  (if (vertical-or-horizontal? line-segment)
    (apply combo/cartesian-product coords)
    (apply map vector coords)))

(defn line-segment->coords [line-segment]
  (->> line-segment
       (apply map vector)
       (map range-2)
       (combine-coords line-segment)))

(defn count-overlaps [coords]
  (->> coords
       frequencies
       vals
       (filter #(> % 1))
       count))

(defn part1 [input]
  (->> input
       parse-input
       (filter vertical-or-horizontal?)
       (mapcat line-segment->coords)
       count-overlaps))

(defn part2 [input]
  (->> input
       parse-input
       (mapcat line-segment->coords)
       count-overlaps))

(comment
  (part1 (slurp (io/resource "day5.txt")))
  (part2 (slurp (io/resource "day5.txt"))))

