(ns advent-of-code.day09
  (:require [clojure.string :as str]))

(def input (slurp "resources/day09.txt"))

(defn parse [input]
  (mapv #(mapv parse-long (re-seq #"-?\d+" %)) (str/split-lines input)))

(defn difference [history]
  (->> (butlast history)
       (map-indexed vector)
       (reduce
        (fn [differences [i v]]
          (conj differences (- (get history (inc i)) v)))
        [])))

(defn extrapolate [history]
  (->> history
       (iterate difference)
       (take-while #(not (every? zero? %)))))

(defn run [input f]
  (->> (parse input)
       (map (fn [history] (f (extrapolate history))))
       (reduce +)))

(defn part1 [input]
  (run input (fn [history]
               (->> history
                    (map last)
                    (reduce +)))))

(defn part2 [input]
  (run input (fn [history]
               (->> history
                    (map first)
                    reverse
                    (reduce #(- %2 %1) 0)))))

(comment
  (part1 input)
  ;; => 1762065988

  (part2 input)
  ;; => 1066
  )
