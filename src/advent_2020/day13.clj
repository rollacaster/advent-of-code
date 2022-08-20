(ns advent-2020.day13
  (:require [clojure.string :as str]))

(def exercise-input "939
7,13,x,x,59,x,31,19")

(def input (slurp "resources/day13.txt"))

(defn parse-input [input]
  (let [[early-depart bus-ids] (str/split-lines input)]
    {:early-depart (parse-long early-depart)
     :bus-ids (mapv parse-long (re-seq #"\d+" bus-ids))}))


(defn part1 [input]
  (let [{:keys [early-depart bus-ids]} (parse-input input)
        [{:keys [next-departure bus-id]}] (->> bus-ids
                                               (map
                                                (fn [bus-id]
                                                  (some (fn [time] (when (int? (/ time bus-id)) {:next-departure time
                                                                                                :bus-id bus-id}))
                                                        (range early-depart ##Inf))))
                                               (sort-by :next-departure))]
    (* (- next-departure early-depart) bus-id)))

(defn parse-input2 [input]
  (let [[_ bus-ids] (str/split-lines input)]
    (keep-indexed (fn [idx d] (when (parse-long d) [(parse-long d) idx])) (re-seq #"\d+|x" bus-ids))))

(defn combined-series [series [new-bus-id idx]]
  (filter
   (fn [x] (= (mod (+ x idx) new-bus-id) 0))
   series))

(defn part2 [input]
  (let [[[start-bus-id] & other-bus-ids] (parse-input2 input)]
    (first
     (reduce
      (fn [series next-bus]
        (let [[n1 n2] (combined-series series next-bus)]
          (range n1 ##Inf (- n2 n1))))
      (range start-bus-id ##Inf start-bus-id)
      other-bus-ids))))

(comment
  (part1 exercise-input)
  (part1 input)
  (part2 exercise-input)
  (part2 input))
