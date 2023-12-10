(ns advent-of-code.day03
  (:require [clojure.string :as str]))

(def input (slurp "resources/day03.txt"))

(defn matches-seq [re s]
  (let [matcher (re-matcher re s)]
    (loop [matches []]
      (if (.find matcher)
        (recur (conj matches {:start (.start matcher)
                              :end (.end matcher)
                              :match (.group matcher)}))
        matches))))

(defn lines-with-adjacents [input]
  (let [lines (str/split-lines input)]
    (for [idx (range (count lines))]
      (cond (= idx 0) [(lines 0) (lines 1)]
            (= idx (dec (count lines))) [(lines idx) (lines (dec idx))]
            :else [(lines idx) (lines (dec idx)) (lines (inc idx))]))))

(defn process-line [[line & adjacents]]
  (let [numbers (->> (conj adjacents line)
                     (mapcat #(matches-seq #"(\d+)" %)))
        gears (matches-seq  #"(\*)" line)]
    (reduce
     (fn [gear-ratio gear]
       (let [adjacent-numbers
             (filter (fn [number]
                       (or (= (inc (:start gear)) (:start number))
                           (= (:start gear) (:end number))
                           (<= (:start number) (:start gear) (:end number))))
                     numbers)]
         (if (= (count adjacent-numbers) 2)
           (+ gear-ratio
              (* (parse-long (:match (first adjacent-numbers)))
                 (parse-long (:match (second adjacent-numbers)))))
           gear-ratio)))
     0
     gears)))

(defn process-line1 [[line & adjacents]]
  (let [numbers (matches-seq #"(\d+)" line)
        symbols (->> (conj adjacents line)
                     (mapcat #(matches-seq #"([^0-9\.])" %)))]
    (->> numbers
         (filter
          (fn [number]
            (some
             (fn [symbol]
               (or (= (dec (:start number)) (:start symbol))
                   (= (:end number) (inc (:end symbol)))
                   (<= (:start number) (:start symbol) (:end number))))
             symbols)))
         (map (comp parse-long :match)))))

(defn part1 [input]
  (let [lines (lines-with-adjacents input)]
    (reduce + (mapcat process-line1 lines))))

(defn part2 [input]
  (let [lines (lines-with-adjacents input)]
    (reduce + (map process-line lines))))


(comment
  (part1 input)
  ;; => 530849
  (part2 input)
  ;; => 84900879
  )
