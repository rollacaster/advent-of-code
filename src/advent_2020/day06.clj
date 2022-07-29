(ns advent-2020.day06
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(def exercise-input "abc

a
b
c

ab
ac

a
a
a
a

b")

(def input (slurp "resources/day06.txt"))

(defn parse-input [input ](str/split input #"\R\R"))

(defn part1 [input]
  (let [groups (parse-input input)]
    (->> groups
         (map (fn [group]
                (-> group
                    (str/replace "\n" "")
                    (str/split #"")
                    set
                    count)))
         (reduce +))))

(defn part2 [input]
  (let [groups (parse-input input)]
    (->> groups
         (map (fn [group]
                (->> group
                     (str/split-lines)
                     (map set)
                     (apply set/intersection)
                     count)))
         (reduce +))))

(comment
  (parse-input exercise-input)
  (part1 exercise-input)
  (parse-input input)
  (part1 input)
  (part2 input))
