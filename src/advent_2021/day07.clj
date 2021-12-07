(ns advent-2021.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn part1 [input]
  (let [input (map #(Integer/parseInt %) (str/split input #","))]
    (->> input
         set
         (map
          (fn [pos]
            (->> input
                 (map (fn [i] (Math/abs (- i pos))))
                 (reduce +))))
         (apply min))))

(defn part2 [input]
  (let [input (map #(Integer/parseInt %) (str/split input #","))]
    (->> (range (apply min input) (inc (apply max input)))
         (map
          (fn [pos]
            (->> input
                 (map (fn [i] (apply + (range 1 (inc (Math/abs (- i pos)))))))
                 (reduce +))))
         (apply min))))

(comment
  (part1 (str/trim (slurp (io/resource "day7.txt"))))
  (part2 (str/trim (slurp (io/resource "day7.txt")))))
