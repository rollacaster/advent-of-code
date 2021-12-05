(ns advent-2021.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (for [coord-str (str/split line #" -> ")]
      (map #(Integer/parseInt %) (str/split coord-str #",")))))

(defn bound [lines]
  (apply max (flatten (map flatten lines))))

(defn draw-grid [max-bound]
  (into
   []
   (for [x (range (inc max-bound))]
     (into [] (for [y (range (inc max-bound))]
                ".")))))

(defn points [part2 [[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2)
    (for [y (range y1
                   ((if (< y1 y2) inc dec) y2)
                   (if (< y1 y2) 1 -1))]
      [x1 y])
    (= y1 y2)
    (for [x (range x1
                   ((if (< x1 x2) inc dec) x2)
                   (if (< x1 x2) 1 -1))]
      [x y1])
    :else (if part2
            (map
             (fn [x y] [x y])
             (range x1 ((if (< x1 x2) inc dec) x2) (if (< x1 x2) 1 -1))
             (range y1 ((if (< y1 y2) inc dec) y2) (if (< y1 y2) 1 -1)))
            [])))

(defn mult-overlaps [grid]
  (count (filter #(and (int? %) (> % 1)) (flatten grid))))

(defn res [part2 input]
  (let [lines (parse-input input)]
    (->> lines
         (reduce
          (fn [grid line]
            (reduce
             (fn [grid [x y]]
               (update-in grid [y x] (fn [v] (if (int? v)
                                              (inc v)
                                              1))))
             grid
             (points part2 line)))
          (draw-grid (bound lines)))
         mult-overlaps)))

(defn part1 [input] (res false input))
(defn part2 [input] (res true input))

(comment
  (part1 (slurp (io/resource "day5.txt")))
  (part2 (slurp (io/resource "day5.txt"))))
