;; # 🎄 Advent of Clerk: Day 8
(ns advent-of-clerk.day-08
  (:require
   [clojure.string :as str]))

(def input (slurp "resources/day08.txt"))

(defn parse-input [input]
  (mapv #(mapv (comp parse-long str) %) (str/split-lines input)))

(defn neighbour-pos [input [x y]]
  [(for [x (range (dec x) -1 -1)] [x y])
   (for [x (range (inc x) (count (first input)))] [x y])
   (for [y (range (dec y) -1 -1)] [x y])
   (for [y (range (inc y) (count input))] [x y])])

(defn visible? [input [x y]]
  (let [current (get-in input [y x])]
    (some
     (fn [neighbours]
       (every? (fn [[x y]] (> current (get-in input [y x])))neighbours))
     (neighbour-pos input [x y]))))

(defn edge? [input [x y]]
  (or (= x 0) (= y 0) (= (dec (count input)) y) (= (dec (count (first input))) x)))

(defn part1 [input]
  (count
   (let [input (parse-input input)]
     (for [y (range 0 (count input))
           x (range 0 (count (first input)))
           :when
           (or (edge? input [x y]) (visible? input [x y]))]
       [x y]))))

(part1 input)

(defn scenic-score [input [x y]]
  (let [current (get-in input [y x])]
    (->> (neighbour-pos input [x y])
         (map
          (fn [neighbours]
            (let [smaller-trees
                  (->> neighbours
                       (take-while (fn [[x y]] (> current (get-in input [y x]))))
                       count)]
              (if (= smaller-trees (count neighbours))
                smaller-trees
                (inc smaller-trees)))))
         (apply *))))

(defn part2 [input]
  (let [input (parse-input input)]
    (apply max
           (for [y (range 0 (count input))
                 x (range 0 (count (first input)))]
             (scenic-score input [x y])))))

(part2 input)
