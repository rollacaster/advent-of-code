(ns advent-2021.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn line-won? [line] (every? #(= % :m) line))

(defn won? [board]
  (when
      (or
       (->> board
            (map (fn [hor-line] (map (fn [[_ mark]] mark) hor-line)))
            (some line-won?))
       (->> board
            (mapcat (fn [hor-line] (map-indexed vector hor-line)))
            (group-by first)
            vals
            (map (fn [vert-line] (map (fn [[_ [_ mark]]] mark ) vert-line)))
            (some line-won?)))
    board))

(defn mark-number [number board]
  (map
   (fn [hor-line]
     (map (fn [[num mark]] (if (= num number) [num :m] [num mark]))hor-line))
   board))

(defn prep-boards [boards]
  (->> boards
       (map
        (fn [board]
          (map
           (fn [line]
             (map
              #(vector % :u)
              (-> line
                  (str/trim)
                  (str/replace "  " " ")
                  (str/split #" "))))
           board)))))

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
