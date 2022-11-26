(ns advent-2020.day22
  (:require [clojure.string :as str]))

(def exercise-input "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(def input (slurp "resources/day22.txt"))

(defn parse-input [input]
  (map #(->> % str/split-lines (drop 1) (mapv parse-long)) (str/split input #"\n\n")))

(defn part1 [input]
  (let [result (let [[p1 p2] (parse-input input)]
                 (loop [p1 p1
                        p2 p2]
                   (cond (empty? p1) p2
                         (empty? p2) p1
                         (> (first p1) (first p2))
                         (recur (conj (subvec p1 1) (first p1) (first p2))
                                (subvec p2 1))
                         :else
                         (recur (subvec p1 1)
                                (conj (subvec p2 1) (first p2) (first p1))))))]
    (reduce + (map-indexed
               (fn [i n]
                 (* (- (count result) i) n))
               result))))

(def infinite "Player 1:
43
19

Player 2:
2
29
14")

(defn subgame [p1 p2]
  (loop [p1 p1
         p2 p2
         prev-games #{}]
    (cond (empty? p1) :p2
          (empty? p2) :p1
          (contains? prev-games #{p1 p2}) :p1
          (and (< (first p1)
                  (dec (count p1)))
               (< (first p2)
                  (dec (count p2))))
          (subgame (subvec p1 1) (subvec p2 1))
          (> (first p1) (first p2))
          (recur (conj (subvec p1 1) (first p1) (first p2))
                 (subvec p2 1)
                 (conj prev-games #{p1 p2}))
          :else
          (recur (subvec p1 1)
                 (conj (subvec p2 1) (first p2) (first p1))
                 (conj prev-games #{p1 p2})))))

(defn part2 [input]
  (let [result (let [[p1 p2] (parse-input input)]
                 (loop [p1 p1
                        p2 p2
                        prev-games #{}]
                   (cond (empty? p1) p2
                         (empty? p2) p1
                         (contains? prev-games #{p1 p2}) :p1
                         (and (<= (first p1)
                                 (dec (count p1)))
                              (<= (first p2)
                                 (dec (count p2))))
                         (if (= (subgame (vec (take (first p1) (subvec p1 1)))
                                         (vec (take (first p2) (subvec p2 1))))
                                :p1)
                           (recur (conj (subvec p1 1) (first p1) (first p2))
                                  (subvec p2 1)
                                  (conj prev-games #{p1 p2}))
                           (recur (subvec p1 1)
                                (conj (subvec p2 1) (first p2) (first p1))
                                (conj prev-games #{p1 p2})))
                         (> (first p1) (first p2))
                         (recur (conj (subvec p1 1) (first p1) (first p2))
                                (subvec p2 1)
                                (conj prev-games #{p1 p2}))
                         :else
                         (recur (subvec p1 1)
                                (conj (subvec p2 1) (first p2) (first p1))
                                (conj prev-games #{p1 p2})))))]
    (reduce + (map-indexed
               (fn [i n]
                 (* (- (count result) i) n))
               result))))

(comment
  (part1 exercise-input)
  (part1 input)
  (part1 infinite)
  (part2 exercise-input)
  (part2 input)
  ;; 32149 - too low
  ;; 34174 - too high
  )
