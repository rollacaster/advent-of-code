(ns advent-2020.day05
  (:require [clojure.string :as str]))

(def input (slurp "resources/day05.txt"))

(defn parse-input [input]
  (str/split-lines input))

(defn col [boarding-pass]
  (loop [possible-cols (range 0 8)
         [pos & rest] boarding-pass]
    (if (nil? pos)
      (first possible-cols)
      (recur
       (case pos
         \L (take (/ (count possible-cols) 2) possible-cols)
         \R (drop (/ (count possible-cols) 2) possible-cols))
       rest))))

(defn seat [boarding-pass]
  (loop [possible-rows (range 0 128)
         [pos & rest] boarding-pass]
    (if (or (= pos \R) (= pos \L))
      [(first possible-rows) (col (conj rest pos))]
      (recur
       (case pos
         \F (take (/ (count possible-rows) 2) possible-rows)
         \B (drop (/ (count possible-rows) 2) possible-rows))
       rest))))

(defn seat-id [boarding-pass]
  (let [[row col] (seat boarding-pass)]
    (+ (* row 8) col)))

(defn part1 [input]
  (->> input
       parse-input
       (map seat-id)
       (apply max)))

(defn part2 [input]
  (let [seats (->> input
                   parse-input
                   (map seat-id)
                   sort
                   (map-indexed vector))]
    (->> seats
         (drop-while (fn [[i seat-id]] (= (second (nth seats (inc i))) (inc seat-id))))
         first
         second
         inc)))

(comment
  (part1 input)
  (part2 input))
