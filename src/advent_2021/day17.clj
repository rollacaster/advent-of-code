(ns advent-2021.day17
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]))

(defn drag [x]
  (cond
    (= x 0) 0
    (> x 0) (dec x)
    (< x 0) (inc x)))

(defn step [[[x y] [vx vy]]]
  [[(+ x vx)
    (+ y vy)]
   [(drag vx)
    (dec vy)]])

(defn parse-group [group]
  (->> group
       (take-last 2)
       (mapv #(Integer/parseInt %))))

(defn parse-target [input]
  (->> input
       (re-seq #"((-?\d*)\.\.(-?\d*))")
       (mapv parse-group)))

(defn in-target [[[x1 x2] [y1 y2]] [x y]] (and (<= x1 x x2) (<= y1 y y2)))

(defn overshoot? [[[x1 x2] [y1 y2]] [x y]] (or (> x x2) (< y y1)))

(defn find-max [target path]
  (when (in-target target (first (step (last path))))
    (apply max (map #(second (first %)) path))))

(defn max-y [target velocity]
  (->> [[0,0] velocity]
       (iterate step)
       (take-while (fn [[pos]]
                     (and (not (overshoot? target pos))
                          (not (in-target target pos)))))
       (find-max target)))

(defn possible-velocities [[[_ x2]]]
  (inc x2))

(defn part1 [input]
  (let [target (parse-target input)]
    (apply
     max
     (filter
      identity
      (for [x (range (possible-velocities target))
            y (range (possible-velocities target))]
        (max-y target [x y]))))))

(defn part2 [input]
  (let [target (parse-target input)]
    (count (filter
            identity
            (for [x (range (possible-velocities target))
                  y (range (- (possible-velocities target))
                           (possible-velocities target))]
              (when (max-y target [x y]) [x y]))))))

(comment
  (part1 (slurp (io/resource "day17.txt")))
  (part2 (slurp (io/resource "day17.txt"))))
