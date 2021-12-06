(ns advent-2021.day06
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn prep-input [input]
  (map #(Integer/parseInt %) (str/split (str/trim input) #",")))


(defn part1 [days input]
  (->> (range days)
       (reduce
        (fn [state _]
          (let [new-fish (count (filter #(< % 0) (map dec state)))]
            (concat
             (map (fn [timer] (if (< (dec timer) 0) 6 (dec timer))) state)
             (repeat new-fish 8))))
        (prep-input input))
       count))

(def count-f
  (memoize
   (fn [day fish]
     (if (= day 0) 1
         (reduce
          +
          (map
           (partial count-f (dec day) fish)
           (if (= fish 0) [6 8] [(dec fish)])))))))

(defn part2 [days input]
  (apply + (map (partial count-f days) (prep-input input))))

(comment
  (part1 80 (slurp (io/resource "day6.txt")))
  (part2 256 (slurp (io/resource "day6.txt"))))
