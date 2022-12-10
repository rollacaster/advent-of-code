;; # 🎄 Advent of Clerk: Day 10
(ns advent-of-clerk.day-10
  (:require [clojure.string :as str]))

(def input (slurp "resources/day10.txt"))

(defn parse-input [input]
  (mapcat (fn [s] (if (str/includes? s "noo")
                   [[:noop]]
                   [[:noop] [:addx (parse-long (re-find #"-?\d+" s))]]))
          (str/split-lines input)))

(defn- step [state [command value]]
  (-> state
      (update 0 inc)
      (update 1 (fn [v] (if (= command :addx) (+ value v) v)))))

(defn steps [input]
  (->> input
       parse-input
       (reductions step [1 1])))

(defn part1 [input]
  (->> [20 60 100 140 180 220]
       (map (fn [i] (apply * (nth (steps input) (dec i)))))
       (apply +)))

(part1 input)

(defn part2 [input]
  (->> (steps input)
       (reduce
        (fn [screen [cycle pos]]
          (str screen (if (<= pos (mod cycle 40) (-> pos inc inc)) "#" ".")))
        "")
       (partition 40)
       (map #(apply str %))))

(part2 input)
