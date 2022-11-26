(ns advent-2020.day11
  (:require
    [clojure.string :as str]))

(def exercise-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def input (slurp "resources/day11.txt"))

(defn vec+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn neighbours-in-dir [pos dir width height]
  (->> [dir pos]
       (iterate (fn [[dir curr]] [dir (vec+ curr dir)]))
       rest
       (map second)
       (take-while (fn [[x y]] (and (>= x 0)
                                   (>= y 0)
                                   (< x width)
                                   (< y height))))))

(defn part-1 [pos dir {:keys [width height seats]}]
  (let [[x y] (vec+ dir pos)]
    (if (and (>= x 0) (>= y 0) (< x width) (< y height)
         (= (get seats [x y]) :occupied-seat))
      1 0)))

(defn part-2 [pos dir {:keys [width height seats]}]
  (let [neighbour-dir
        (->> (neighbours-in-dir pos dir width height)
             (map (fn [neighbour] (seats neighbour)))
             (drop-while (fn [n] (= n :floor)))
             first)]
    (if (= neighbour-dir :occupied-seat) 1 0)))

(defn occupied-seats [pos neighbour-fn input]
  (->> [[-1 -1] [0 -1] [1 -1]
        [-1 0] [1 0]
        [-1 1] [0 1] [1 1]]
       (map (fn [dir] (neighbour-fn pos dir input)))
       (reduce +)))

(defn next-step [neighbour-fn max-seat {:keys [seats] :as input}]
  (assoc input :seats
         (->> seats
              (mapcat
               (fn [[pos type]]
                 [pos
                  (let [occupied-seats (occupied-seats pos neighbour-fn input)]
                    (cond (and (= type :empty-seat) (= occupied-seats 0)) :occupied-seat
                          (and (= type :occupied-seat) (>= occupied-seats max-seat)) :empty-seat
                          :else type))]))
              (apply hash-map))))

(defn position [width idx]
  [(mod idx width) (int (/ idx width))])

(defn parse-input [input]
  (let [input-seq (str/split-lines input)
        width (count (first input-seq))
        height (count input-seq)]
    {:seats (->> input-seq
                 (apply str)
                 (map-indexed (fn [idx seat] [(position width idx) (case seat \L :empty-seat \. :floor nil)]))
                 (apply map vector)
                 (apply zipmap))
     :width width
     :height height}))

(defn solve [input max-seat neighbour-fn]
  (->> (reduce
        (fn [pre _]
          (let [res (next-step neighbour-fn max-seat pre)]
            (if (= pre res)
              (reduced res)
              res)))
        (parse-input input)
        (range))
       :seats
       (filter (fn [[_ type]] (= :occupied-seat type)))
       count))

(comment
  (solve exercise-input 4 part-1)
  37
  (solve input 4 part-1)
  2386
  (solve exercise-input 5 part-2)
  26
  (solve input 5 part-2)
  2091)
