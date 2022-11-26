(ns advent-2021.day18
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.zip :as z]))

(defn explode-loc [n]
  (->> (z/vector-zip n)
       (iterate z/next)
       (take-while (fn [[_ m]]
                     (and (not= m :end)
                          (not= (count (:pnodes m)) 5))))
       last))

(defn explodes? [n]
  (vector?
   (z/node (explode-loc n))))

(defn add-to-left [loc summand]
  (if (= (z/left loc) nil)
    (if (= (z/up loc) nil)
      (explode-loc (z/root loc))
      (add-to-left (z/up loc) summand))
    (if (vector? (z/node (z/left loc)))
      (loop [loc (-> loc z/left z/down z/rightmost)]
        (if (vector? (-> loc z/node))
          (recur (-> loc z/down z/rightmost))
          (-> loc
              (z/edit (fn [n] (+ n summand)))
              z/root
              explode-loc)))
      (-> loc
          z/left
          (z/edit (fn [n] (+ n summand)))
          z/root
          explode-loc))))

(defn add-to-right [loc summand]
  (if (= (z/right loc) nil)
    (if (= (z/up loc) nil)
      (explode-loc (z/root loc))
      (add-to-right (z/up loc) summand))
    (if (vector? (z/node (z/right loc)))
      (loop [loc (-> loc z/right z/down z/leftmost)]
        (if (vector? (-> loc z/node))
          (recur (-> loc z/down z/leftmost))
          (-> loc
              (z/edit (fn [n] (+ n summand)))
              z/root
              explode-loc)))
      (-> loc
          z/right
          (z/edit (fn [n] (+ n summand)))
          z/root
          explode-loc))))

(defn explode [n]
  (let [loc (explode-loc n)]
    (-> loc
        (add-to-left (z/node (z/down loc)))
        (add-to-right (z/node (z/right (z/down loc))))
        (z/replace 0)
        z/root)))

(defn splits? [n]
  (>= (apply max (flatten n)) 10))

(defn split [n]
  (loop [loc (z/vector-zip n)]
    (if (and (number? (z/node loc)) (>= (z/node loc) 10))
      (z/root (z/edit loc (fn [n] [(int (Math/floor (/ n 2))) (int (Math/ceil (/ n 2)))])))
      (recur (z/next loc)))))

(defn reduce-snail [n]
  (cond
    (explodes? n) (explode n)
    (splits? n) (split n)
    :else n))

(defn addition [a b]
  [a b])

(defn magnitude [[l r]]
  (+ (* 3 (if (vector? l) (magnitude l) l))
     (* 2 (if (vector? r) (magnitude r) r))))

(defn part1 [summands]
  (magnitude (reduce (fn [sum summand]
                       (loop [n (addition sum summand)]
                         (let [reduced (reduce-snail n)]
                           (if (= n reduced)
                             n
                             (recur reduced)))))
                     summands)))

(defn part2 [summands]
  (->> (for [x summands
             y summands]
         (when (not= x y)
           [[x y] [y x]]))
       (mapcat identity)
       (map part1)
       (apply max)))

(comment
  (part1 (map read-string (str/split-lines (slurp (io/resource "day18.txt")))))
  (part2 (map read-string (str/split-lines (slurp (io/resource "day18.txt"))))))






