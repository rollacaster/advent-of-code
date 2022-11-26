(ns advent-2021.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  (map parse-long (str/split-lines input)))

(defn part-1 [data]
  (->> data
       (partition 2 1)
       (filter #(apply < %))
       count))

(defn part-2 [data]
  (->> data
       (partition 3 1)
       (map #(apply + %))))

(comment
  (part-1 (parse-input (slurp (io/resource "day01.txt"))))
  (-> (slurp (io/resource "day01.txt"))
      parse-input
      part-2
      part-1))
