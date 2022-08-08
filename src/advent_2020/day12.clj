(ns advent-2020.day12)

(def exercise-input "F10
N3
F7
R90
F11")

(def input (slurp "resources/day12.txt"))

(defn parse-input [input]
  (->> input
       (re-seq #"(\w)(\d+)")
       (map rest)
       (mapv (fn [[action value]] [(keyword action) (parse-long value)]))))

(defn keep-position [a _] a)

(def left-rotations
  [[+ keep-position]
   [keep-position +]
   [- keep-position]
   [keep-position -]])

(def right-rotations
  (vec (reverse left-rotations)))

(defn forward [{:keys [direction] :as state} value]
  (update state :position
          (fn [position]
            (mapv
             (fn [position direction]
               (direction position value))
             position
             direction))))

(defn rotate [direction rotations value]
  (nth
   rotations
   (mod
    (+ (.indexOf rotations direction) (/ value 90))
    (count rotations))))

(defn manhatten [{[x y] :position}]
  (+ (Math/abs x) (Math/abs y)))

(defn part1 [input]
  (->> input
       parse-input
       (reduce
        (fn [state [action value]]
          (case action
            :N (update-in state [:position 1] + value)
            :S (update-in state [:position 1] - value)
            :E (update-in state [:position 0] + value)
            :W (update-in state [:position 0] - value)
            :L (update state :direction rotate left-rotations value)
            :R (update state :direction rotate right-rotations value)
            :F (forward state value)))
        {:position [0 0]
         :direction [+ keep-position]})
       manhatten))

(defn rotate-left [[x y] new-direction]
  (case new-direction
    90 [(- y) x]
    180 [(- x) (- y)]
    270 [y (- x)]))

(defn rotate-right [[x y] new-direction]
  (case new-direction
    90 [y (- x)]
    180 [(- x) (- y)]
    270 [(- y) x]))

(defn part2 [input]
  (->> input
       parse-input
       (reduce
        (fn [{[wx wy] :waypoint :as state} [action value]]
          (case action
            :N (update-in state [:waypoint 1] + value)
            :S (update-in state [:waypoint 1] - value)
            :E (update-in state [:waypoint 0] + value)
            :W (update-in state [:waypoint 0] - value)
            :L (update state :waypoint rotate-left value)
            :R (update state :waypoint rotate-right value)
            :F (update state :position (fn [[x y]] [(+ x (* wx value)) (+ y (* wy value))]))))
        {:waypoint [10 1]
         :position [0 0]})
       manhatten))

(comment
  (part1 input)
  (part2 input))
