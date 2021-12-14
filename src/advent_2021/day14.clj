(ns advent-2021.day14
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn prep [input]
  (let [[template steps] (str/split input #"\n\n")]
    [template
     (apply hash-map (mapcat #(str/split % #" -> ") (str/split-lines steps)))]))

(defn step [template steps]
  (apply str
         (conj
          (mapv (fn [[a c]]
                  (str
                   a
                   (steps (apply str [a c]))))
                (partition 2 1 template))
          (last template))))

(defn part1 [input]
  (let [[template steps] (prep input)
        freqs (->> (loop [n 10
                          template template]
                     (if (= n 0)
                       template
                       (recur (dec n) (step template steps))))
                   frequencies
                   vals)]
    (- (apply max freqs)
       (apply min freqs))))


(defn s [rules init]
  (apply
   merge-with
   +
   (mapcat
    (fn [[k j]] (map (fn [n] (hash-map n (or j 1))) (get rules k)))
    init)))

(defn part2 [input]
  (let [[template steps] (prep input)
        rules (apply hash-map
                     (mapcat (fn [[[fa fb] t]] [(str fa fb) [(str fa t) (str t fb)]])
                             steps))
        init (apply
              merge-with
              +
              (map
               #(hash-map (apply str %) 1)
               (partition 2 1
                          template)))
        f (partial s rules)
        freqs (set/map-invert
               (update
                (apply
                 merge-with
                 +
                 (map
                  (fn [[[a] k]] (hash-map (str a) k))
                  (last (take 41 (iterate f init)))))
                (str (last template))
                inc))]
    (-
     (apply max (keys freqs))
     (apply min (keys freqs)))))

(comment
  (part1 (slurp (io/resource "day14.txt")))
  (part2 (slurp (io/resource "day14.txt"))))




