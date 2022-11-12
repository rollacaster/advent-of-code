(ns advent-2020.day24
  (:require [clojure.string :as str]))

(def input (slurp "resources/day24.txt"))

(defn- add
  ([v] v)
  ([[x1 y1] [x2 y2]]
   [(+ x1 x2) (+ y1 y2)])
  ([v1 v2 & vs]
   (apply add (add v1 v2) vs)))

(def directions
  {"e"  [1  0]
   "w"  [-1 0]
   "ne" [1 -1]
   "nw" [0 -1]
   "se" [0  1]
   "sw" [-1 1]})

(defn- parse-line [line]
  (->> line
       (re-seq #"e|w|se|sw|ne|nw")
       (map directions)
       (reduce add)))

(defn- parse-black-tiles [input]
  (->> input
       str/split-lines
       (map parse-line)
       frequencies
       (filter (comp odd? second))))

(defn part1 [input]
  (->> input
       parse-black-tiles
       count))

(defn- neighbours [pos]
  (map #(add pos %) (vals directions)))

(defn- step [state]
  (set
   (for [[tile n] (frequencies (mapcat #(neighbours %) state))
         :when (if (state tile)
                 (#{1 2} n)
                 (= n 2))]
     tile)))

(defn part2 [input]
  (let [result (->> input
                    parse-black-tiles
                    (map first)
                    set
                    (iterate step))]
    (-> result
        (nth 100)
        count)))
(comment
  (part1 input)
  (part2 input))
