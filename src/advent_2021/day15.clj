(ns advent-2021.day15
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.data.priority-map :as prio-map]))

(defn prep-input [input]
  (map #(Integer/parseInt (str %)) (apply str (str/split-lines input))))

(defn nodes [dim]
  (for [y (range dim)
        x (range dim)]
    [x y]))

(def neigbours [[1 0] [0 1] [-1 0] [0 -1]])

(defn coord [dim pos]
  (let [y (quot pos dim)
        x (- pos (* y dim))]
    [x y]))
(defn edges [dim input]
  (apply
   hash-map
   (mapcat
    (fn [pos]
      (let [[x y] (coord dim pos)]
        [[x y]
         (->> neigbours
              (map (fn [[nx ny]] [(+ nx x) (+ y ny)]))
              (remove (fn [[nx ny]] (or (< nx 0) (>= nx dim) (< ny 0) (>= ny dim))))
              (map (fn [[nx ny]] [(nth input (+ (* dim ny) nx)) [nx ny]])))]))
    (range (* dim dim)))))


(defn dijkstra [dim input]
  (let [edges (edges dim input)]
    (loop [paths (apply
                  prio-map/priority-map-keyfn
                  first
                  (mapcat (fn [x] [x (if (= x [0 0]) [0] [Integer/MAX_VALUE])]) (nodes dim)))
           visited #{}]
      (let [curr (first (peek paths))]
        (if (= curr [(dec dim) (dec dim)])
          (get paths [(dec dim) (dec dim)])
          (recur
           (dissoc
            (reduce
             (fn [paths [weight to]]
               (if (< (+ (first (get paths curr)) weight)
                      (+ (first (get paths to))))
                 (assoc paths to [(+ weight (first (get paths curr))) curr])
                 paths))
             paths
             (remove #(visited (second %)) (get edges curr)))
            curr)
           (conj visited curr)))))))

(defn part1 [input]
  (first (dijkstra (count (take-while #(not= % \newline) input))
                   (prep-input input))))

(defn biggie [dim input]
  (for [idx (range (* 25 (count input)))]
    (let [y (quot idx (* dim 5))
          x (- idx (* y (* dim 5)))
          ny (mod y dim)
          nx (mod x dim)]
      (-> (+ (nth input (+ nx (* ny dim)))
             (quot x dim)
             (quot y dim))
          dec
          (mod 9)
          inc))))


(defn part2 [input]
  (let [dim (count (take-while #(not= % \newline) input))
        big-input (biggie dim (prep-input input))]
    (first (dijkstra (* dim 5) big-input))))


(comment
  (part1 (slurp (io/resource "day15.txt")))
  (part2 (slurp (io/resource "day15.txt"))))



