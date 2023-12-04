(ns advent-of-code.day02
  (:require [clojure.string :as str]))

(def input (slurp "resources/day02.txt"))

(defn parse [input]
  (map (fn [line]
         (let [[[_ id sets-of-cubes]] (re-seq #"Game (\d+):(.*)" line)]
           {:id (parse-long id)
            :sets-of-cubes (mapv
                    (fn [game-set]
                      (into {}
                            (mapv (fn [cube-count-and-color]
                                    (let [[_ cube-count color] (re-find #"(\d+) (\w+)" cube-count-and-color)]
                                      [color (parse-long cube-count)]))
                                  (str/split game-set #","))))
                    (str/split sets-of-cubes #";"))}))
       (str/split-lines input)))

(defn part1 []
  (->> (parse input)
       (filter
        (fn [{:keys [sets-of-cubes]}]
          (every?
           (fn [game-set]
             (and
              (or (nil? (get game-set "red"))
                  (<= (get game-set "red") 12))
              (or (nil? (get game-set "green"))
                  (<= (get game-set "green") 13))
              (or (nil? (get game-set "blue"))
                  (<= (get game-set "blue") 14))))
           sets-of-cubes)))
       (map :id)
       (reduce +)))

(defn part2 []
  (->> (parse input)
       (map
        (fn [{:keys [sets-of-cubes]}]
          (apply *
                 (map
                  (fn [k]
                    (apply max (map #(get % k 0) sets-of-cubes)))
                  ["blue" "green" "red"]))))
       (apply +)))

(part1)
;; => 2176

(part2)
;; => 63700
