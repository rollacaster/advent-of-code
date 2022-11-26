(ns advent-2020.day15
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (map parse-long (str/split input #",")))

(defn run [input max]
  (let [input (parse-input input)]
    (reduce
     (fn [{:keys [round spoken last]} _]
       (if (= round max)
         (reduced last)
         {:round (inc round)
          :spoken (assoc spoken last round)
          :last (if (nil? (get spoken last)) 0 (- round (get spoken last)))}))
     {:round (count input)
      :spoken (zipmap
               input
               (range 1 (count input)))
      :last (last input)}
     (range))))

(defn part-1 [input]
  (run input 2020))

(defn part-2 [input]
  (run input 30000000))

(comment
  (part-1 "0,3,6")
  ;; => 436
  (part-1 "1,3,2")
  ;; => 1
  (part-1 "2,1,3")
  ;; => 10
  (part-1 "1,2,3")
  ;; => 27
  (part-1 "2,3,1")
  ;; => 78
  (part-1 "3,2,1")
  ;; => 438
  (part-1 "3,1,2")
  ;; => 1836
  (part-1 "6,19,0,5,7,13,1")
  ;; => 468
  (part-2 "0,3,6")
  (part-2 "1,3,2")
  (part-2 "2,1,3")
  (part-2 "1,2,3")
  (part-2 "2,3,1")
  (part-2 "3,2,1")
  (part-2 "3,1,2")
  (part-2 "6,19,0,5,7,13,1"))
