;; # 🎄 Advent of Clerk: Day 14
(ns advent-of-clerk.day-14
  (:require
   [clojure.string :as str]))

(def input (slurp "resources/day14.txt"))

(defn parse-input [input]
  (mapv (fn [line] (mapv #(mapv parse-long (str/split % #",")) (re-seq #"[0-9,]+" line))) (str/split-lines input)))

(defn- generate-line [line-points [x2 y2]]
  (let [[x1 y1] (last line-points)
        x-steps (- x1 x2)
        y-steps (- y1 y2)]
    (concat (butlast line-points)
            (cond (not (zero? x-steps))
                  (cond->> (map (fn [x] [x y1]) (range (min x1 x2) (inc (max x1 x2))))
                    (pos? x-steps) reverse)
                  (not (zero? y-steps))
                  (cond->> (map (fn [y] [x1 y]) (range (min y1 y2) (inc (max y1 y2))))
                    (pos? y-steps) reverse)))))

(defn moves [[x y]] [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]])

(defn run [game limit]
  (let [lines (->> input
                   parse-input
                   (mapcat (fn [line] (reduce generate-line (take 1 line) (rest line))))
                   set)]
    (game lines (limit (->> lines (map last) (apply max))) [500 0])))

(defn part1 [lines abyss start]
  (loop [sand #{}
         [x y] start]
    (if (= y abyss)
      (count sand)
      (let [[down left right] (moves [x y])]
        (cond
          (and (not (sand down)) (not (lines down))) (recur sand down)

          (and (not (sand left)) (not (lines left))) (recur sand left)

          (and (not (sand right)) (not (lines right))) (recur sand right)

          :else (recur (conj sand [x y]) start))))))

(run part1 identity)

(defn part2 [lines floor start]
  (loop [sand #{}
         [x y] start]
    (let [[down left right] (moves [x y])
          floor? (= (inc y) floor)]
      (cond
        (and (not (sand down)) (not (lines down)) (not floor?)) (recur sand down)

        (and (not (sand left)) (not (lines left)) (not floor?)) (recur sand left)

        (and (not (sand right)) (not (lines right)) (not floor?)) (recur sand right)

        (= [x y] start) (inc (count sand))

        :else
        (recur (conj sand [x y]) start)))))

(run part2 #(+ 2 %))
