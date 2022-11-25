(ns advent-2020.day09
  (:require [clojure.string :as str]))

(def input (slurp "resources/day09.txt"))

(defn parse-input [input] (map parse-long (str/split-lines input)))

(defn part1 [input]
  (let [preamble-length 25]
    (loop [input (parse-input input)]
      (let [preamble (set (take preamble-length input))
            [to-test] (drop preamble-length input)
            allowed-numbers (set
                             (mapcat
                              (fn [a]
                                (map
                                 (fn [b] (+ a b))
                                 (disj preamble a)))
                              preamble))]
        (if (allowed-numbers to-test)
          (recur (rest input))
          to-test)))))

(defn cumulated-sum [input]
  (map-indexed
   (fn [idx _]
     (reduce + (take idx input)))
   input))

(defn- find-cont-set-idxs [input to-sum]
  (loop [i 0]
    (let [input (drop i input)
          cont-set (take-while #(<= % to-sum) (cumulated-sum input))]
      (if (= to-sum (last cont-set))
        [i (+ i (count cont-set))]
        (recur (inc i))))))

(defn part2 [input]
  (let [to-sum (part1 input)
        input (parse-input input)
        [smallest largest] (find-cont-set-idxs input to-sum)
        cont-set (subvec (vec input) smallest (dec largest))]
    (+ (apply min cont-set) (apply max cont-set))))

(comment
  (part1 input)
  (part2 input))
