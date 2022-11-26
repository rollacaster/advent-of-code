(ns advent-2021.day22
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-int [n] (Integer/parseInt n))

(defn parse-coords [coords]
  (->> coords
       (map parse-int)
       (partition 2)
       (map #(vector (first %) (second %)))
       (zipmap [:x :y :z])))

(defn parse-line [line]
  (let [[op & coords] (->> (re-seq #"(on|off) x=(-?\d*)\.\.(-?\d*),y=(-?\d*)\.\.(-?\d*),z=(-?\d*)\.\.(-?\d*)" line)
                         first
                         rest)]
    {:op (keyword op) :coords (parse-coords coords)}))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-line)))

(defn cubes [{[x1 x2] :x
              [y1 y2] :y
              [z1 z2] :z}]
  (if
   (or (< x1 -50) (> x2 50)
       (< y1 -50) (> y2 50)
       (< z1 -50) (> z2 50))
    []
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))
          z (range z1 (inc z2))]
      [x y z])))

(defn part1 [input]
  (count
   (reduce
    (fn [on-cubes {:keys [op coords]}]
      (if (= op :on)
        (into on-cubes (cubes coords))
        (apply disj on-cubes (cubes coords))))
    #{}
    (parse-input input))))

(defn count-cubes [{[x1 x2] :x
                    [y1 y2] :y
                    [z1 z2] :z}]
  (* (- (inc x2) x1) (- (inc y2) y1) (- (inc z2) z1)))

(defn intersection-cube [{[x1 x2] :x
                          [y1 y2] :y
                          [z1 z2] :z}
                         {:keys [x y z]}]
  (let [x1 (max x1 (first x)) x2 (min x2 (second x))
        y1 (max y1 (first y)) y2 (min y2 (second y))
        z1 (max z1 (first z)) z2 (min z2 (second z))]
    (when (and (<= x1 x2) (<= y1 y2) (<= z1 z2))
      {:x [x1 x2]
       :y [y1 y2]
       :z [z1 z2]})))

(defn part2 [input]
  (->> (loop [[curr & rest] (parse-input input)
              cubes []]
         (if (= curr nil)
           cubes
           (recur
            rest
            (cond-> (reduce
                     (fn [cubes cube]
                       (if-let [intersection
                                (intersection-cube
                                 (:coords curr)
                                 (:coords cube))]
                         (conj
                          cubes
                          {:op (if (= (:op cube) :on) :off :on)
                           :coords intersection})
                         cubes))
                     cubes
                     cubes)
              (= (:op curr) :on) (conj curr)))))
       (map
        (fn [{:keys [op coords]}]
          ((if (= op :on) + -)
           (count-cubes coords))))
       (apply +)))

(comment
  (part1 (slurp (io/resource "day22.txt")))
  (part2 (slurp (io/resource "day22.txt"))))
