(ns advent-2021.day13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn prep-input [input]
  (let [[dots instructions] (str/split input #"\n\n")]
    [(mapv #(mapv (fn [n] (Integer/parseInt n)) (str/split % #",")) (str/split-lines dots))
     (mapv #(let [[command param] (str/split (last (str/split % #"fold along ")) #"=")]
             [command (Integer/parseInt param)])
          (str/split-lines instructions))]))

(defn part1 [input]
  (let [[dots commands] (prep-input input)]
    (->> commands
         (take 1)
         (reduce
          (fn [dots [command coord]]
            (map
             (if (= command "y")
               (fn [[x y]]
                 (if (> y coord)
                   [x (- y (* 2 (- y coord)))]
                   [x y]))
               (fn [[x y]]
                 (if (> x coord)
                   [(- x (* 2 (- x coord))) y]
                   [x y])))
             dots))
          dots)
         distinct
         count)))

(defn part2 [input]
  (let [[dots commands] (prep-input input)
        coords (->> commands
                    (reduce
                     (fn [dots [command coord]]
                       (map
                        (if (= command "y")
                          (fn [[x y]]
                            (if (> y coord)
                              [x (- y (* 2 (- y coord)))]
                              [x y]))
                          (fn [[x y]]
                            (if (> x coord)
                              [(- x (* 2 (- x coord))) y]
                              [x y])))
                        dots))
                     dots)
                    distinct
                    vec)]
    (->> (for [y (range (inc (apply max (map second coords))))]
           (for [x (range (inc (apply max (map first coords))))]
             (if (some #(= % [x y]) coords) "#" ".")))
         (map #(apply str %)))))

(comment
  (part1 (slurp (io/resource "day13.txt")))
  (part2 (slurp (io/resource "day13.txt"))))
