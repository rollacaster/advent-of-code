;; # 🎄 Advent of Clerk: Day 11
(ns advent-of-clerk.day-11
  (:require [clojure.string :as str]))

(def input (slurp "resources/day11.txt"))

(defn- parse-monkey [state monkey]
  (let [[monkey start operation & check] (str/split-lines monkey)
        monkey-id (parse-long (re-find #"\d+" monkey))
        [divisble-by then else] (mapv (fn [line] (->> line (re-find #"\d+") parse-long)) check)
        [op summand] (rest (re-find #"= old ([+*]) (old|\d+)" operation))]
    (-> state
        (update :worry-mod * divisble-by)
        (update :state assoc monkey-id {:items (mapv (comp parse-long second) (re-seq #"(\d+)" start))
                                        :inspect #((case op "+" + "*" *) % (or (parse-long summand) %))
                                        :throw-to #(if (zero? (mod % divisble-by)) then else )}))))

(defn parse-input [input]
  (reduce parse-monkey {:worry-mod 1 :state {}} (str/split input #"\n\n")))

(defn- step [state monkey-id reduce-worry worry-mod]
  (let [{:keys [items inspect throw-to]} (get state monkey-id)]
    (reduce
     (fn [state item]
       (let [worry-level (reduce-worry (inspect item) worry-mod)]
         (-> state
             (update-in [monkey-id :items] subvec 1)
             (update-in [(throw-to worry-level) :items] conj worry-level)
             (update-in [:inspected monkey-id] (fnil inc 0)))))
     state
     items)))

(defn run [input rounds reduce-worry]
  (let [{:keys [state worry-mod]} (parse-input input)
        monkey-ids (sort (keys state))]
    (->> (cycle monkey-ids)
         (take (* rounds (count monkey-ids)))
         (reduce (fn [state monkey-id] (step state monkey-id reduce-worry worry-mod)) state)
         :inspected
         vals
         (sort >)
         (take 2)
         (apply *))))

(run input 20 (fn [worry-level _] (int (/ worry-level 3))))
(run input 10000 (fn [worry-level worry-mod] (mod worry-level worry-mod)))
