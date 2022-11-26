(ns advent-2020.day18
  (:require [clojure.string :as str]))

(def input (slurp "resources/day18.txt"))

(defn parse-input [input]
  (->> input
       (remove #(= % \space))
       (map (comp #(case % ")" ")" "(" "(" (read-string %)) str))))

(defn next-rest [exp]
  (let [[x & xs] exp]
    (case x
      "(" (loop [[x & xs] xs
                 res '()
                 parenthesis-level 1]
            (cond
              (= parenthesis-level 0) [(reverse (drop 1 res)) (when x (conj xs x))]
              (= x "(") (recur xs (conj res x) (inc parenthesis-level))
              (= x ")") (recur xs (conj res x) (dec parenthesis-level))
              :else (recur xs (conj res x) parenthesis-level)))
      [x xs])))

(defn calc [op exp parse-exp]
  (loop [[x & xs] exp
         current-result []]
    (if (nil? x)
      current-result
      (cond
        (= x "(") (let [[next rest] (next-rest (conj xs x))]
                    (recur rest (conj current-result (parse-exp next))))
        (and (symbol? x) (contains? op (name x)))
        (if (= (first xs) "(")
          (let [[next rest] (next-rest xs)]
            (recur rest
                   (conj (pop current-result)
                         ((resolve x) (or (last current-result) current-result) (parse-exp next)))))
          (recur (rest xs)
                 (conj (pop current-result)
                       ((resolve x) (or (last current-result) current-result) (first xs)))))
        :else (recur xs (conj current-result x))))))

(defn parse-exp [exp]
  (first (calc #{"+" "*"} exp parse-exp)))

(defn part1 [input]
  (->> input
       str/split-lines
       (map (comp eval parse-exp parse-input))
       (reduce +)))

(defn parse-exp2 [exp]
  (first (calc #{"*"} (calc #{"+"} exp parse-exp2) parse-exp2)))

(defn part2 [input]
  (->> input
       str/split-lines
       (map (comp eval parse-exp2 parse-input))
       (reduce +)))

(comment
  (part1 input)
  (part2 input))
