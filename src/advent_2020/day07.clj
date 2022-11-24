(ns advent-2020.day07
  (:require [clojure.string :as str]))

(def input (slurp "resources/day07.txt"))

(defn- parse-bag [bag]
  (let [[_ number color] (re-find #"(\d) (.*) bags?.?" bag)]
    [color (parse-long number)]))

(defn parse-input [input]
  (mapcat
   (fn [line]
     (let [[bag-color contents] (str/split line #" bags contain ")]
       [bag-color
        (let [bags (str/split contents #", ")]
          (if (= bags ["no other bags."])
            []
            (mapv parse-bag bags)))]))
   (str/split-lines input)))


(defn walk-bags [bags contents]
  (reduce (fn [inside-bags [color]]
            (concat
             (walk-bags bags (bags color))
             (conj inside-bags color)))
          []
          contents))

(defn part1 [input]
  (let [bags (apply hash-map (parse-input input))]
    (->> bags
         (remove #(= (first %) "shiny gold"))

         (map (fn [[color contents]] [color (set (walk-bags bags contents))]))
         (filter (fn [[_ inside-colors]] (inside-colors "shiny gold")))
         count)
    (->> bags
         (remove #(= (first %) "shiny gold"))

         (map (fn [[color contents]] [color (set (walk-bags bags contents))]))
         (filter (fn [[_ inside-colors]] (inside-colors "shiny gold")))
         count)))

(defn count-bags [bags contents]
  (reduce (fn [bags-count [color bag-count]]
            (+ bags-count
               bag-count
               (* bag-count (count-bags bags (bags color)))))
          0
          contents))

(defn part2 [input]
  (let [bags (apply hash-map (parse-input input))]
    (count-bags bags (bags "shiny gold"))))

(comment
  (part1 input)
  (part2 input))
