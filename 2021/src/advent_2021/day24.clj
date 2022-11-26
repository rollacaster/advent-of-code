(ns advent-2021.day24
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-arg [arg]
  (try
    (Integer/parseInt arg)
    (catch NumberFormatException _
      (keyword arg))))

(defn parse-line [line]
  (let [[op & args] (str/split line #" ")]
    {:op (keyword op) :args (mapv parse-arg args)}))

(defn parse-input [input]
  (mapv parse-line (str/split-lines input)))

(def vars {:w 0 :x 0 :y 0 :z 0})
(defn read-b [state b]
  (if (number? b) b (get state b)))
(defmulti execute2 (fn [_ {:keys [op]}] op))
(defmethod execute2 :inp [state {[a b] :args}]
  (assoc state a b))
(defmethod execute2 :add [state {[a b] :args}]
  (assoc
   state
   a
   (+ (get state a)
      (read-b state b))))
(defmethod execute2 :mul [state {[a b] :args}]
  (assoc
   state
   a
   (* (get state a)
      (read-b state b))))
(defmethod execute2 :div [state {[a b] :args}]
  (assoc
   state
   a
   (quot (get state a)
         (read-b state b))))
(defmethod execute2 :mod [state {[a b] :args}]
  (assoc
   state
   a
   (mod (get state a)
        (read-b state b))))
(defmethod execute2 :eql [state {[a b] :args}]
  (assoc
   state
   a
   (if (= (get state a) (read-b state b)) 1 0)))

(defn add-inps [instructions insps]
  (loop [i 0
         insps insps
         instructions (vec instructions)]
    (if (= i (dec (count instructions)))
      instructions
      (recur
       (inc i)
       (if (= (:op (nth instructions i)) :inp)
         (rest insps)
         insps)
       (if (= (:op (nth instructions i)) :inp)
         (update instructions i
                 (fn [instruction]
                   (update instruction :args conj (first insps))))
         instructions)))))

(defn run [input params]
  (map-indexed
   vector
   (reductions
    (partial reduce execute2)
    vars
    (partition 18 (add-inps input params)))))

(comment
;; 3-2 delta 1
;; 6-1 delta 8
;; 5-4 delta 6
;; 8-7 delta 0
;; 12-9 delta 5
;; 11-10 delta 2
;; 13-0 delta 4
  (run
   (parse-input (slurp (io/resource "day24.txt")))
   [5 1 9 8 3 9 9 9 9 4 7 9 9 9])
  (run
   (parse-input (slurp (io/resource "day24.txt")))
   [1 1 2 1 1 7 9 1 1 1 1 3 6 5]))
