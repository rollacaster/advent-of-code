(ns advent-2021.day09
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn prep-input [input]
  (mapv #(mapv (fn [n] (Integer/parseInt (str n))) %) (str/split-lines input)))

(defn get-neighbours-coord [input row col]
  (let [left  (dec col)
        right (inc col)
        top    (dec row)
        bottom  (inc row)]
    (cond-> []
      (>= left 0) (conj [row left])
      (>= top 0) (conj [(dec row) col])
      (< right (count (first input))) (conj [row (inc col)])
      (< bottom (count input)) (conj [(inc row) col]))))

(defn low-point? [input row col]
  (every?
   (fn [[nrow ncol]]
     (< (get-in input [row col])
        (get-in input [nrow ncol])))
   (get-neighbours-coord input row col)))

(defn part1 [input]
  (let [input (prep-input input)]
    (->> (for [[row line] (map-indexed vector input)
               col (range (count line))]
           (when (low-point? input row col)
             (get-in input [row col])))
         (filter identity)
         (map inc)
         (apply +))))

(defn basin [input cur row col]
  (if (get cur [row col])
    cur
    (let [val (get-in input [row col])]
      (if (not= val 9)
        (reduce
         (fn [cur [nrow ncol]]
           (basin input cur nrow ncol))
         (assoc cur [row col] val)
         (get-neighbours-coord input row col))
        cur))))

(defn part2 [input]
  (let [input (prep-input input)]
    (->> (for [[row line] (map-indexed vector input)
               col (range (count line))]
           (when (low-point? input row col)
             (count (vals (basin input {} row col)))))
         (filter identity)
         sort
         reverse
         (take 3)
         (apply *))))

(comment
  (part1 (slurp (io/resource "day9.txt")))
  (part2 (slurp (io/resource "day9.txt"))))
