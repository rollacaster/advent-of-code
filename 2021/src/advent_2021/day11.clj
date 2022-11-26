(ns advent-2021.day11
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn prep-input [input]
  (mapv
   (fn [row]
     (mapv #(Integer/parseInt (str %)) row))
   (str/split-lines input)))

(defn get-neighbours-coord [input row col]
  (let [left  (dec col)
        right (inc col)
        top    (dec row)
        bottom  (inc row)]
    (cond-> []
      (>= left 0) (conj [row left])
      (>= top 0) (conj [top col])
      (< right (count (first input))) (conj [row right])
      (< bottom (count input)) (conj [bottom col])
      (and (>= left 0) (>= top 0))
      (conj [top left])
      (and (< right (count (first input))) (>= top 0))
      (conj [top right])
      (and (< right (count (first input))) (< bottom (count input)))
      (conj [bottom right])
      (and (>= left 0) (< bottom (count input)))
      (conj [bottom left]))))

(defn flashes [state]
  (->> (range (count state))
       (mapcat (fn [y] (mapv
                       (fn [x] (if (and (number? (get-in state [x y])) (> (get-in state [x y]) 9))
                                [x y]
                                []))
                       (range (count (get state y))))))
       (filter #(> (count %) 0))))

(defn flash-it [state]
  (reduce
   (fn [state [x y]]
     (reduce
      (fn [state [x y]]
        (update-in state [x y] (fn [n] (if (number? n) (inc n) n))))
      (assoc-in state [x y] :flashed)
      (get-neighbours-coord state x y)))
   state
   (flashes state)))

(defn part1 [input]
  (let [flash-count (atom 0)]
    (doall
     (reductions
      (fn [state _]
        (loop [state (flash-it (mapv (fn [row] (mapv inc row)) state))]
          (if (> (count (flashes state)) 0)
            (recur (flash-it state))
            (mapv (fn [row] (mapv (fn [n] (if (= n :flashed) (do (swap! flash-count inc) 0) n)) row)) state))))
      (prep-input input)
      (range 100)))
    @flash-count))

(defn all-flash [state]
  (every? #(= % 0) (flatten state)))

(defn part2 [input]
  (reduce
   (fn [state step]
     (loop [state (flash-it (mapv (fn [row] (mapv inc row)) state))]
       (if (> (count (flashes state)) 0)
         (recur (flash-it state))
         (let [next (mapv (fn [row] (mapv (fn [n] (if (= n :flashed) 0 n)) row)) state)]
           (if (all-flash next)
             (reduced (inc step))
             next)))))
   (prep-input input)
   (range)))

(comment
  (part1 (slurp (io/resource "day11.txt")))
  (part2 (slurp (io/resource "day11.txt"))))

