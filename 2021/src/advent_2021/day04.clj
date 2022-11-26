(ns advent-2021.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-board [board]
  (map #(re-seq #"\d+" %) board))

(defn parse-input [input]
  (let [[[input] & boards]
        (->> (str/split input #"\n")
             (partition-by #(= % ""))
             (remove #(= % '(""))))]
    {:draws (str/split input #",")
     :boards (map parse-board boards)}))

(defn board-won? [board]
  (some (fn [line] (every? #(= % :marked) line)) board))

(defn won? [board]
  (when (or
         (board-won? board)
         (->> board
              (apply map vector)
              board-won?))
    board))

(defn mark-number [number board]
  (for [line board]
    (for [num line]
      (if (= num number) :marked num))))

(defn unmarked-numbers-sum [board]
  (->> board
       (mapcat (fn [hor-line] (remove (fn [num] (= num :marked)) hor-line)))
       (map parse-long)
       (apply +)))

(defn part1 [input]
  (let [{:keys [draws boards]} (parse-input input)]
    (reduce
     (fn [boards number]
       (let [updated-boards (map (fn [board] (mark-number number board)) boards)]
         (if (some won? updated-boards)
           (reduced (* (parse-long number)
                       (unmarked-numbers-sum (some won? updated-boards))))
           updated-boards)))
     boards
     draws)))

(defn part2 [input]
  (let [{:keys [draws boards]} (parse-input input)]
    (reduce
     (fn [boards number]
       (let [updated-boards (map (fn [board] (mark-number number board)) boards)]
         (if (every? won? updated-boards)
            (reduced (* (parse-long number)
                        (unmarked-numbers-sum
                         (mark-number number (first (filter (complement won?) boards))))))
            updated-boards)))
     boards
     draws)))

(comment
  (part1 (slurp (io/resource "day4.txt")))
  (part2 (slurp (io/resource "day4.txt"))))
