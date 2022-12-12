;; # 🎄 Advent of Clerk: Day 12
(ns advent-of-clerk.day-12
  (:require [clojure.string :as str]))

(def input (slurp "resources/day12.txt"))

(defn find-coords [lines to-find]
  (for [y (range 0 (count lines))
        x (range 0 (count (first lines)))
        :when (= (nth (nth lines y) x) to-find)]
    [x y]))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:start (first (find-coords lines \S))
     :starts (find-coords lines \a)
     :target (first (find-coords lines \E))
     :elevations
     (apply hash-map
      (mapcat identity
              (for [y (range 0 (count lines))
                    x (range 0 (count (first lines)))
                    :let [char (nth (nth lines y) x)]]
                [[x y] (case char \S 0 \E 26 (- (int char) 96))])))}))

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn edges [elevations position cost]
  (apply hash-map (->> [[0 1] [0 -1] [1 0] [-1 0]]
                       (map (fn [delta] (add position delta)))
                       (filter #(<= (get elevations % 100) (inc (get elevations position))))
                       (mapcat (fn [pos] [pos (inc cost)])))))


(defn dijkstra-search
  [initial final? generate-moves]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[cost state] [0 initial]
           visited #{}]
      (if (nil? state)
        ##Inf
        (do
          (when (not (visited state))
            (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
              (when (not (visited nxt-state))
                (.add to-visit [nxt-cost nxt-state]))))
          (if (final? state)
            cost
            (recur (.poll to-visit)
                   (conj visited state))))))))

(defn part1 [input]
  (let [{:keys [elevations start target]} (parse-input input)]
    (dijkstra-search start (fn [pos] (= pos target)) (fn [[position cost]] (edges elevations position cost)))))

(part1 input)

(defn part2 [input]
  (let [{:keys [elevations starts target]} (parse-input input)]
    (->> starts
         (map #(dijkstra-search % (fn [pos] (= pos target)) (fn [[position cost]] (edges elevations position cost))))
         (apply min))))

(part2 input)
