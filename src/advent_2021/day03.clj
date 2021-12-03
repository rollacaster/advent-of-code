(ns advent-2021.day03
  (:require
   [advent-2021.utils :refer [read-file]]
   [clojure.string :as str]))

(def input
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(defn binary [decimal]
  (if (= decimal 0)
    0
    (+ (mod decimal 2) (* 10 (binary (quot decimal 2))))))

(defn read-binary [binary-string]
  (read-string (str "2r" binary-string)))

(defn group-occurences [input]
  (apply map
         (fn [& args]
           (->> args
                (group-by identity)
                (map (fn [[n ns]] [n (count ns)]))))
         input))

(defn part1 [input]
  (let [gamma
        (->> input
             group-occurences
             (map (fn [[[n1 c1] [n2 c2]]] (if (> c1 c2) n1 n2)))
             str/join)
        epsilon (->> gamma
                     (map (fn [i] (if (= i \0) \1 \0)))
                     (apply str))]
    (* (read-binary gamma) (read-binary epsilon))))

(defn c [f pos input]
  (if (= (count input) 1)
    (first input)
    (c f (inc pos) (filter #(= (nth % pos) (nth (map f (group-occurences input)) pos)) input))))

(defn part2 [input]
  (->> [(fn [[[n1 c1] [n2 c2]]]
          (cond
            (not c2) n1 
            (= c1 c2) \0
            (> c1 c2) n2
            :else n1))
        (fn [[[n1 c1] [n2 c2]]]
          (cond
            (not c2) n1 
            (= c1 c2) \1
            (> c1 c2) n1
            :else n2))]
       (map (fn [f] (read-binary ((partial c f 0) (read-file "day3.txt")))))
       (apply *)))

(comment
  (part1 (read-file "day3.txt"))
  (part2 (read-file "day3.txt")))




