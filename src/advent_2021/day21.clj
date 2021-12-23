(ns advent-2021.day21
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [rhizome.viz :as tree]))

(def input "Player 1 starting position: 4
Player 2 starting position: 8")

(defn parse-position [position-str]
  (Integer/parseInt (last (re-matches #"Player \d starting position: (\d*)" position-str))))

(defn parse-input [input]
  (map parse-position (str/split-lines input)))

(defn move [n]
  (-> n dec (mod 10) inc))

(defn part1 [input]
  (let [[p1-pos p2-pos] (parse-input input)]
    (loop [dice (range 1 ##Inf)
           turn 0
           p1-pos p1-pos
           p1-score 0
           p2-pos p2-pos
           p2-score 0]
      (let [p1 (move (+ p1-pos (apply + (take 3 dice))))
            p2 (move (+ p2-pos (apply + (take 3 (drop 3 dice)))))
            p1-score (+ p1-score p1)
            p2-score (+ p2-score p2)]
        (cond
          (>= p1-score 1000) (* (- p2-score p2) (first (drop 2 dice)))
          (>= p2-score 1000) (* p1-score (first (drop 5 dice)))
          :else
          (recur
           (drop 6 dice)
           (inc turn)
           p1
           p1-score
           p2
           p2-score))))))

(def quantum-throw
  (for [x [1 2 3]
        y [1 2 3]
        z [1 2 3]]
    [x y z]))

(defn next-move [{:keys [pos total games throws]}]
  (mapv
   (fn [[throw times-thrown]]
     (let [new-pos (move (+ pos throw))
           new-total (+ total new-pos)
           game-state {:pos new-pos
                       :total new-total
                       :games
                       (+ games times-thrown)
                       :throws (inc throws)}]
       (cond-> game-state
         (< new-total 21) (assoc :children (next-move game-state)))))
   (->> quantum-throw
        (map #(apply + %))
        (frequencies))))

(defn throws-to-win [start-pos]
  (->> {:children
        (next-move {:total 0
                     :pos start-pos
                     :throws 0
                     :games 0})}
       (tree-seq :children :children)
       (filter (complement :children))
       (group-by :throws)
       (mapcat (fn [[throws ends]] [throws (apply + (map :games ends))]))
       (apply hash-map)))

(defn part2-fail [input]
  (let [[p1-pos p2-pos] (parse-input input)]
    (max (apply
          +
          (for [p (throws-to-win p1-pos)]
            (->> (throws-to-win p2-pos)
                 (filter (fn [[throws]] (>= throws (first p))))
                 (map second)
                 (apply +)
                 (* (second p)))))
         (apply
          +
          (for [p (throws-to-win p2-pos)]
            (->> (throws-to-win p1-pos)
                 (filter (fn [[throws]] (> throws (first p))))
                 (map second)
                 (apply +)
                 (* (second p))))))))

(def game
  (memoize (fn [p1-pos p1-score p2-pos p2-score]
             (if
              (>= p2-score 21) [0 1]
              (reduce #(mapv + %1 %2)
                      (for [dice quantum-throw]
                        (let [p1 (move (+ p1-pos (apply + dice)))
                              p1-score (+ p1-score p1)]
                          (reverse (game p2-pos p2-score p1 p1-score)))))))))

(defn part2-cheat [input]
  (let [[p1-pos p2-pos] (parse-input input)]
    (apply
     max
     (game p1-pos 0 p2-pos 0))))

(comment
  (part1 input)
  (part1 (slurp (io/resource "day21.txt")))
  (part2-cheat input)
  (part2-cheat (slurp (io/resource "day21.txt"))))


