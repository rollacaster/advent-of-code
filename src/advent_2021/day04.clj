(ns advent-2021.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn board-won? [board]
  (->> board
       (map (fn [line] (map (fn [[_ mark]] mark) line)))
       (some (fn [line] (every? #(= % :m) line)))))

(defn won? [board]
  (when
      (or
       (board-won? board)
       (->> board
            (apply map vector)
            board-won?))
    board))

(defn mark-number [number board]
  (for [line board]
    (for [[num mark] line]
      (if (= num number) [num :m] [num mark]))))

(defn prep-boards [boards]
  (for [board boards]
    (for [line board]
      (for [cell (-> line
                     (str/trim)
                     (str/split #"\s+"))]
        [cell :u]))))

(defn unmarked-numbers-sum [board]
  (->> board
       (mapcat (fn [hor-line] (filter (fn [[num mark]] (= mark :u)) hor-line)))
       (map #(Integer/parseInt (first %)))
       (apply +)))

(defn part1 [input]
  (let [[[input] & boards] (->> (str/split input #"\n")
                                (partition-by #(= % ""))
                                (remove #(= % '(""))))
        boards (prep-boards boards)
        inputs (str/split input #",")]
    (reduce
     (fn [boards number]
       (let [updated-boards (map (partial mark-number number) boards)]
         (if (some won? updated-boards)
           (reduced (* (Integer/parseInt number)
                       (unmarked-numbers-sum (some won? updated-boards))))
           updated-boards)))
     boards
     inputs)))

(defn part2 [input]
  (let [[[input] & boards] (->> (str/split input #"\n")
                                (partition-by #(= % ""))
                                (remove #(= % '(""))))
        boards (prep-boards boards)
        inputs (str/split input #",")]
    (reduce
     (fn [boards number]
       (let [updated-boards (map (partial mark-number number) boards)]
         (if (every? won? updated-boards)
           (reduced (* (Integer/parseInt number)
                       (unmarked-numbers-sum
                        (mark-number number (first (filter (complement won?) boards))))))
           updated-boards)))
     boards
     inputs)))

(comment
  (part1 (slurp (io/resource "day4.txt")))
  (part2 (slurp (io/resource "day4.txt"))))
