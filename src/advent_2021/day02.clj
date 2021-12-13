(ns advent-2021.day02
  (:require [advent-2021.utils :refer [read-file]]
            [clojure.string :as str]))

(defn prep [data]
  (->> data
       (map #(str/split % #" "))
       (map (fn [[command amount]] [command (Integer/parseInt amount)]))))

(defn part-1 [data]
  (let [[pos depth]
        (reduce (fn [[pos depth] [command amount]]
                  (case command
                    "forward"
                    [(+ pos amount) depth]
                    "up"
                    [pos (- depth amount)]
                    "down"
                    [pos (+ depth amount)]))
                [0 0]
                (prep data))]
    (* pos depth)))

(defn part-2 [data]
  (let [[pos depth]
        (reduce (fn [[pos depth aim] [command amount]]
                  (case command
                    "down"
                    [pos depth (+ aim amount)]
                    "up"
                    [pos depth (- aim amount)]
                    "forward"
                    [(+ pos amount) (+ depth (* aim amount)) aim]))
                [0 0 0]
                (prep data))]
    (* pos depth)))

(comment
  (part-1 (read-file "day2.txt"))
  (part-2 (read-file "day2.txt")))

