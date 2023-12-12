(ns advent-of-code.day04
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp "resources/day04.txt"))

(defn part1 [input]
  (->> (str/split-lines input)
       (map (fn [s]
              (let [[winning-line my-number] (str/split s #"\|")
                    [_ winning-numbers] (str/split winning-line #"Card\s+\d+\:")
                    intersection (count
                                  (set/intersection
                                   (set (re-seq #"\d+" winning-numbers))
                                   (set (re-seq #"\d+" my-number))))]
                (if (pos? intersection)
                  (Math/pow 2 (dec intersection))
                  0))))
       (reduce +)))

(defn part2 [input]
  (let [lines (str/split-lines input)
        init-cards (reduce
                    (fn [cards n]
                      (assoc cards (inc n) 1))
                    {}
                    (range (count (str/split-lines input))))]
    (->> lines
         (reduce (fn [cards line]
                   (let [[winning-line my-number] (str/split line #"\|")
                         [card-line winning-numbers] (str/split winning-line #":")
                         card-number (parse-long (re-find #"\d+" card-line))
                         matches (count
                                  (set/intersection
                                   (set (re-seq #"\d+" winning-numbers))
                                   (set (re-seq #"\d+" my-number))))]
                     (reduce
                      (fn [cards match]
                        (update cards match (fn [n] (+ (get cards card-number) (or n 0)))))
                      cards
                      (range (inc card-number) (inc (+ matches card-number))))))
                 init-cards)
         vals
         (reduce +))))

(comment
  (part1 input)
  ;; => 27454.0
  (part2 input)
  ;; => 6857330
  )
