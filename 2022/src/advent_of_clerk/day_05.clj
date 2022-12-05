;; # 🎄 Advent of Clerk: Day 5
(ns advent-of-clerk.day-05
  (:require [clojure.string :as str]))

(def input (slurp "resources/day05.txt"))

(defn parse-input [input]
  (let [[crates instructions] (str/split input #"\n\n")]
    {:crates
     (->> crates
          str/split-lines
          (apply map vector)
          (map #(apply str %))
          (filter #(re-find #"[A-Z1-9]" %))
          (mapv #(vec (re-seq #"[A-Z]" %))))
     :instructions
     (->> instructions
          (re-seq #"\d+")
          (map parse-long)
          (partition 3)
          (mapv #(-> % vec (update 1 dec) (update 2 dec))))}))

(defn part1 [crates [n from to]]
  (reduce
   (fn [crates _]
     (-> crates
         (update from subvec 1)
         (update to #(into (subvec (nth crates from) 0 1) %))))
   crates
   (range 0 n)))

(defn part2 [crates [n from to]]
  (mapv
   (fn [[idx crate]]
     (cond
       (= from idx) (subvec crate n)
       (= to idx) (into (subvec (nth crates from) 0 n) crate)
       :else crate))
   (map-indexed vector crates)))

(defn run [input f]
  (let [{:keys [crates instructions]} (parse-input input)]
    (->> instructions
         (reduce f crates)
         (map first)
         (apply str))))

(run input part1)
(run input part2)
