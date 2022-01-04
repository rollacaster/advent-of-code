(ns advent-2021.day02
  (:require [clojure.java.io :as io]))

(defn parse-input [input]
  (->> input
       (re-seq #"[\d\w]+")
       (partition 2)
       (map (fn [[c v]] [c (parse-long v)]))))

(defn part-1 [input]
  (->> (parse-input input)
       (reduce (fn [[pos depth] [command amount]]
                 (case command
                   "forward" [(+ pos amount) depth]
                   "up" [pos (- depth amount)]
                   "down" [pos (+ depth amount)]))
               [0 0])
       (apply *)))

(defn part-2 [input]
  (->> (parse-input input)
       (reduce (fn [[pos depth aim] [command amount]]
                 (case command
                   "down" [pos depth (+ aim amount)]
                   "up" [pos depth (- aim amount)]
                   "forward" [(+ pos amount) (+ depth (* aim amount)) aim]))
               [0 0 0])
       drop-last
       (apply *)))

(comment
  (part-1 (slurp (io/resource "day02.txt")))
  (part-2 (slurp (io/resource "day02.txt"))))



