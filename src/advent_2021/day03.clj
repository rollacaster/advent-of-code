(ns advent-2021.day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-binary [binary-string]
  (read-string (str "2r" binary-string)))

(defn parse-input [input]
  (str/split-lines input))

(defn part1 [input]
  (->> (parse-input input)
       (apply map vector)
       (map (comp
             #(map first %)
             #(sort-by second %)
             vec
             frequencies))
       (apply map vector)
       (map (comp read-binary
                  #(apply str %)))
       (apply *)))

(defn bit-criteria [rating-type idx [a b]]
  (cond
    (= (count a) (count b))
    (filter
     #(= (nth % idx) (if (= rating-type :oxy) \1 \0))
     (concat a b))
    ((if (= rating-type :oxy) > <) (count a) (count b))
    a
    :else
    b))

(defn matching-numbers [type idx numbers]
  (->> numbers
       (group-by #(nth % idx))
       (map second)
       (bit-criteria type idx)))

(defn find-rating [type numbers]
  (loop [numbers numbers
         idx 0]
    (let [numbers (matching-numbers type idx numbers)]
      (if (= (count numbers) 1)
        (first numbers)
        (recur numbers (inc idx))))))

(defn part2 [input]
  (->> (parse-input input)
       ((juxt (partial find-rating :oxy)
              (partial find-rating :scrub)))
       (map (comp read-binary parse-long))
       (apply *)))

(comment
  (part1 (slurp (io/resource "day3.txt")))
  (part2 (slurp (io/resource "day3.txt"))))




