(ns advent-2020.day03
  (:require [clojure.string :as str]))

(def exercise-input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def input (slurp "resources/day03.txt"))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv (fn [tree-line] (mapcat str (repeat tree-line))))))

(defn get-tree [forest x y]
  (nth (nth forest y) x))

(defn part1 [input]
  (let [forest (parse-input input)]
    (loop [[x y] [0 0]
           tree-count 0]
      (let [[nx ny] [(+ x 3) (+ y 1)]]
        (if (> ny (dec (count forest)))
          tree-count
          (recur [nx ny]
                 (+ tree-count (if (= (get-tree forest nx ny) \#) 1 0))))))))

(defn part2 [input]
  (let [forest (parse-input input)
        slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (->> (for [slope slopes]
           (loop [[x y] [0 0]
                  tree-count 0]
             (let [[sx sy] slope
                   [nx ny] [(+ x sx) (+ y sy)]]
               (if (> ny (dec (count forest)))
                 tree-count
                 (recur [nx ny]
                        (+ tree-count (if (= (get-tree forest nx ny) \#) 1 0)))))))
         (apply *))))
(comment
  (part1 exercise-input)
  (part1 input)
  (part2 exercise-input)
  (part2 input))
