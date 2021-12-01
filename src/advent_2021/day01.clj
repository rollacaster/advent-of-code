(ns advent-2021.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn part-1 [data]
  (last
   (reduce (fn [[last-measurment larger-count] measurement]
             (if (< last-measurment measurement)
               [measurement (inc larger-count)]
               [measurement larger-count]))
           [(Integer/MAX_VALUE) 0]
           data)))

(defn part-2 [data]
  (->> data
       (map-indexed vector)
       (map
        (fn [[idx number]]
          (when
              (< idx (- (count data) 2))
            (+ number
               (nth data (inc idx))
               (nth data (+ idx 2))))))
       (filter identity)))

(comment
  (let [data (->> (slurp (io/resource "day01.txt"))
               str/split-lines
               (map (fn [n] (Integer/parseInt n))))]
    [(part-1 data)
     (part-1 (part-2 data))]))
