;; # 🎄 Advent of Clerk: Day 6
(ns advent-of-clerk.day-06)

(def input (slurp "resources/day06.txt"))

(defn run [input length]
  (->> input
       (partition length 1)
       (take-while #(not (apply distinct? %)))
       count
       (+ length)))

(defn part1 [input]
  (run input 4))

(defn part2 [input]
  (run input 14))

(part1 input)

(part2 input)
