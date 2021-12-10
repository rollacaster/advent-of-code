(ns advent-2021.day10
  (:require
   [clojure.string :as str]
   [clojure.zip :as z]
   [clojure.java.io :as io]))

(def params {\( \)
             \[ \]
             \{ \}
             \< \>})

(def opening? (set (keys params)))

(def points {\) 3
             \] 57
             \} 1197
             \> 25137})

(defn part1 [input]
  (->> input
       str/split-lines
       (map
        (fn [line]
          (->> line
               (reduce
                (fn [res param]
                  (if (opening? param)
                    (-> res
                        (z/insert-child [param])
                        z/down)
                    (if (not= (params (first (z/node res))) param)
                      (reduced [:err param])
                      (z/remove res))))
                (z/vector-zip [[]])))))
       (filter (fn [err] (= (first err) :err)))
       (map (comp points second))
       (apply +)))

(def points2 {\) 1
             \] 2
             \} 3
             \> 4})

(defn part2 [input]
  (let [res (->> input
         str/split-lines
         (map
          (fn [ line]
            (->> line
                 (reduce
                  (fn [res param]
                    (if (opening? param)
                      (-> res
                          (z/insert-child [param])
                          z/down)
                      (if (not= (params (first (z/node res))) param)
                        (reduced [:err param])
                        (z/remove res))))
                  (z/vector-zip [[]])))))
         (remove (fn [err] (= (first err) :err)))
         (map #(map (comp points2 params) (flatten (z/root %))))
         (map #(reduce (fn [res n] (+ (* res 5) n)) 0 %))
         sort)]
    (nth res (quot (count res) 2))))

(comment
  (part1 (slurp (io/resource "day10.txt")))
  (part2 (slurp (io/resource "day10.txt"))))










