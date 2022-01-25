(ns advent-2021.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-input [input]
  (map #(Integer/parseInt %) (str/split (str/trim input) #",")))

(defn run [input cost-fn]
  (->> (range (apply min input) (inc (apply max input)))
       (map
        (fn [pos]
          (->> input
               (map (fn [i] (cost-fn i pos)))
               (reduce +))))
       (apply min)))

(defn part1 [input]
  (let [input (parse-input input)]
    (run input (fn [from to] (Math/abs (- from to))))))

(defn part2 [input]
  (let [input (parse-input input)]
    (run input (fn [from to] (/ (* (Math/abs (- from to))
                                  (inc (Math/abs (- from to))))
                               2)))))

(comment
  (part1 (slurp (io/resource "day7.txt")))
  (part2 (slurp (io/resource "day7.txt"))))
