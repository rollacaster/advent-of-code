(ns advent-of-code.day08
  (:require [clojure.string :as str]))

(def input (slurp "resources/day08.txt"))

(defn parse [input]
  (let [[instruction _ & nodes] (str/split-lines input)
        nodes (into {}
                    (map
                     (fn [n] (let [[node left right] (re-seq #"\w{3}" n)]
                              [node [left right]]))
                     nodes))]
    [instruction nodes]))

(defn take-turn [node next]
  (if (= next \R) (second node) (first node)))

(defn run [input start-node end-node]
  (let [[instruction nodes] (parse input)
        start-nodes (->> nodes
                         (filter (fn [[node]] (str/ends-with? node start-node)))
                         (map first))]
    (for [start-node start-nodes]
      (loop [[next & rest] instruction
             current-node start-node
             step 1]
        (let [next-node (take-turn (nodes current-node) next)]
          (if (str/ends-with? next-node end-node)
            step
            (cond (nil? rest) (recur instruction next-node (inc step))
                  :else (recur rest next-node (inc step)))))))))

(defn part1 [input]
  (first (run input "AAA" "ZZZ")))

(defn gcd [a b] (if (zero? b) a (recur b, (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))
(defn lcmv [& v] (reduce lcm v))

(defn part2 [input]
  (apply lcmv (run input "A" "Z")))

(comment
  (part1 input)
  ;; => 21883
  (part2 input)
  ;; => 12833235391111
  )
