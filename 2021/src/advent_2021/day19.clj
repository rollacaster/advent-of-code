(ns advent-2021.day19
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-coord [coord-str]
  (mapv #(Integer/parseInt %) (str/split coord-str #",")))

(defn parse-scanner [scanner-str]
  (set (mapv parse-coord (str/split-lines (str/replace scanner-str #"--- scanner \d* ---\n" "")))))

(defn parse-input [input]
  (mapv parse-scanner (str/split input #"\n\n")))

(defn orientations [[x y z]]
  (for [[x y z] [[x y z] [y z x] [z x y] [x z y] [z y x] [y x z]]
        [sx sy sz] [[+ + +] [+ - -] [- + -] [- - +] [+ - +] [+ + -] [- + +] [- - -]]]
    [(sx x) (sy y) (sz z)]))

(defn find-station-position [beacons1 beacons2]
  (some
   (fn [beacons-oriented]
     (when-let [pos
                (->> (for [b1 beacons1 b2 beacons-oriented]
                       (map - b1 b2))
                     frequencies
                     (some
                      (fn [[pos count]]
                        (when (>= count 12) pos))))]
                [pos beacons-oriented]))
   (->> beacons2
        (map orientations)
        (apply map vector))))

(defn part1 [input]
  (count (loop [stations (rest (parse-input input))
                res (first (parse-input input))]
           (if (empty? stations)
             res
             (let [station (first stations)
                   [station-position beacons-oriented] (find-station-position res station)]
               (if station-position
                 (recur
                  (rest stations)
                  (into res
                        (mapv #(mapv + station-position %) beacons-oriented)))
                 (recur (concat (rest stations) [station]) res)))))))

(defn manhatten [v1 v2]
  (apply + (map - v1 v2)))

(defn part2 [input]
  (let [station-positions (loop [stations (rest (parse-input input))
                                 station-positions [[0 0]]
                                 res (first (parse-input input))]
                            (if (empty? stations)
                              station-positions
                              (let [station (first stations)
                                    [station-position beacons-oriented] (find-station-position res station)]
                                (if station-position
                                  (recur
                                   (rest stations)
                                   (conj station-positions station-position)
                                   (into res
                                         (mapv #(mapv + station-position %) beacons-oriented)))
                                  (recur (concat (rest stations) [station])
                                         station-positions
                                         res)))))]
    (apply max
           (for [s1 station-positions
                 s2 station-positions]
             (manhatten s1 s2)))))



(comment
  (part1 (slurp (io/resource "day19.txt")))
  (part2 (slurp (io/resource "day19.txt"))))







