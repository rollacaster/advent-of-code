(ns advent-2020.day17
  (:require [clojure.string :as str]))

(def exercise-input ".#.
..#
###")

(def input (slurp "resources/day17.txt"))

(defn parse-input [input extra-dimensions]
  (set (for [[y line] (map-indexed vector (str/split-lines input))
             [x cube] (map-indexed vector line)
             :when (= cube \#)]
         (apply conj [x y] extra-dimensions))))

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [more (cart (rest colls))
          x (first colls)]
      (cons x more))))

(defn directions [world]
  (let [dimensions (count (first world))]
    (->> (cart (repeat dimensions [-1 0 1]))
         (remove #(= % (repeat dimensions 0))))))

(defn step [actives]
  (let [directions (directions actives)]
    (->> actives
         (mapcat (fn [active] (map (fn [direction] (mapv + active direction)) directions)))
         frequencies
         (keep
          (fn [[pos neighbour-count]]
            (cond
              (and (actives pos) (or (= neighbour-count 2) (= neighbour-count 3))) pos
              (= neighbour-count 3) pos)))
         set)))

(defn run [actives]
  (-> (iterate step actives)
      (nth 6)
      count))

(defn part1 [input]
  (run (parse-input input [0])))

(defn part2 [input]
  (run (parse-input input [0 0])))

(comment
  (part1 exercise-input)
  (part1 input)
  (part2 exercise-input)
  (part2 input))
