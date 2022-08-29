(ns advent-2020.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def exercise-input "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(def input (slurp "resources/day16.txt"))

(defn- parse-rule [[_ name & ranges]]
  {:name name
   :ranges (->> ranges
                (map parse-long)
                (partition-all 2))})

(defn parse-input [input]
  (let [[rules ticket nearby] (str/split input #"\n\n")
        parsed-rules (re-seq #"([\w ]+)\: (\d+)-(\d+) or (\d+)-(\d+)" rules)]
    {:rules (mapv parse-rule parsed-rules)
     :ticket (->> ticket
                  (re-seq #"\d+")
                  (map parse-long))
     :nearby (->> nearby
                  (re-seq #"\d+")
                  (map parse-long)
                  (partition-all (count parsed-rules)))}))

(defn- valid-range? [[from to] value] (<= from value to))
(defn- valid-rule [rule value] (some #(valid-range? % value) (:ranges rule)))
(defn- valid-value [value rules] (some #(valid-rule % value) rules))
(defn- invalid-values [ticket rules] (remove #(valid-value % rules) ticket))

(defn part1 [input]
  (let [{:keys [rules nearby]} (parse-input input)]
    (->> nearby
         (mapcat #(invalid-values % rules))
         (reduce +))))

(defn- invalid-ticket [ticket rules]
  (some #(not (valid-value % rules)) ticket))

(defn- valid-rule2 [rule values]
  (every? #(valid-rule rule %) values))

(defn departue-rule? [rule] (str/starts-with? rule "departure"))

(defn find-rule [found-rules [idx possible-rules]]
  (let [rule (first (set/difference possible-rules (set (map second found-rules))))]
    (conj found-rules [idx rule])))

(defn part2 [input]
  (let [{:keys [rules ticket nearby]} (parse-input input)]
    (->> nearby
         (remove #(invalid-ticket % rules))
         (apply map vector)
         (map-indexed (fn [idx values] [idx (set (keep (fn [rule] (when (valid-rule2 rule values) (:name rule))) rules))]))
         (sort-by #(count (second %)))
         (reduce find-rule [])
         (keep (fn [[idx rule]] (when (departue-rule? rule) (nth ticket idx))))
         (reduce *))))

(comment
  (part1 exercise-input)
  (part1 input)
  (part2 input))
