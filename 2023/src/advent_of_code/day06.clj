(ns advent-of-code.day06
  (:require [clojure.string :as str]))

(def input "Time:        38     67     76     73
Distance:   234   1027   1157   1236")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [line] (map parse-long (re-seq #"\d+" line))))
       (apply map vector)))

(defn ways-to-win [total-time distance-to-beat]
  (->> (range 1 total-time)
       (filter (fn [speed]
                 (> (* (- total-time speed) speed)
                    distance-to-beat)))
       count))

(defn part1 [input]
  (->> (parse-input input)
       (map (fn [[time distance]] (ways-to-win time distance)))
       (apply *)))

(defn parse-input2 [input]
  (->> (str/split-lines input)
       (map (fn [line] (parse-long (str/join (re-seq #"\d+" line)))))))

(defn part2 [input]
  (apply ways-to-win (parse-input2 input)))

(comment
  (part1 input)
  ;; => 303600
  (part2 input)
  ;; => 23654842
  )
