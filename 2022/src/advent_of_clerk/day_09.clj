;; # 🎄 Advent of Clerk: Day 9
(ns advent-of-clerk.day-09
  (:require
   [clojure.string :as str]))

(def input (slurp "resources/day09.txt"))

(defn- add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn- step [head-pos [x y]]
  (or
   (some
    (fn [step-pos]
      (when (= (add [x y] step-pos) head-pos)
        (case step-pos
          [0 2] [x (inc y)]
          ([1 2] [2 2] [2 1]) [(inc x) (inc y)]
          ([2 0]) [(inc x) y]
          ([2 -1] [2 -2] [1 -2]) [(inc x) (dec y)]
          [0 -2] [x (dec y)]
          ([-1 -2] [-2 -2] [-2 -1]) [(dec x) (dec y)]
          [-2 0] [(dec x) y]
          ([-2 1] [-2 2] [-1 2]) [(dec x) (inc y)])))
    [[0 2] [1 2] [2 2] [2 1] [2 0] [2 -1]
     [2 -2] [1 -2] [0 -2] [-1 -2] [-2 -2]
     [-2 -1] [-2 0] [-2 1] [-2 2] [-1 2]])
   [x y]))

(defn- parse-command [command]
  (let [[_ dir steps] (re-find #"(\w) (\d+)" command)]
    (repeat (parse-long steps) dir)))

(defn- move [[x y] dir]
  (case dir
    "R" [(inc x) y]
    "L" [(dec x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))

(defn- update-positions [dir updated-postions position]
  (conj updated-postions
        (if (empty? updated-postions)
          (move position dir)
          (step (last updated-postions) position))))

(defn- apply-command [state dir]
  (let [updated-positions (reduce (partial update-positions dir) [] (:positions state))]
    (-> state
        (assoc :positions updated-positions)
        (update :all-last-postitions conj (last updated-positions)))))

(defn run [input length]
  (->> input
       str/split-lines
       (reduce
        (fn [state command] (reduce apply-command state (parse-command command)))
        {:positions (vec (repeat (inc length) [0 0])) :all-last-postitions #{}})
       :all-last-postitions
       count))

(run input 1)
;; => 6311

(run input 9)
;; => 2482
