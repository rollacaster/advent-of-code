(ns advent-2021.day06
  (:require
   [clojure.java.io :as io]))

(defn prep-input [input]
  (->> input (re-seq #"\d") (map parse-long)))

(defn next-day [state]
  (reduce-kv
   (fn [next day fish-count]
     (cond-> next
       (= day 0)
       (assoc 6 (+ fish-count (get state 7 0))
              8 fish-count)
       (= day 7) (assoc 6 (+ fish-count (get state 0 0)))
       (and (not= day 0) (not= day 7)) (assoc (dec day) fish-count)))
   {}
   state))

(defn calc [input day-count]
  (->> input
       prep-input
       frequencies
       (iterate next-day)
       (take (inc day-count))
       last
       (map second)
       (apply +)))

(defn part1 [input]
  (calc input 80))

(defn part2 [input]
  (calc input 256))

(comment
  (part1 (slurp (io/resource "day6.txt")))
  (part2 (slurp (io/resource "day6.txt"))))
