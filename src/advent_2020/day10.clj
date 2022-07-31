(ns advent-2020.day10
  (:require [clojure.string :as str]))

(def exercise-input "16
10
15
5
1
11
7
19
6
12
4")

(def exercise-input-2 "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(def input (slurp "resources/day10.txt"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-long)))

(defn part1 [input]
       (let [{:keys [d1 d3]}
             (->> input
                  parse-input
                  sort
                  (reduce (fn [{:keys [volt] :as c} adapter]
                            (cond-> (assoc c :volt adapter)
                              (= (- adapter volt) 1) (update :d1 inc)
                              (= (- adapter volt) 3) (update :d3 inc)))
                          {:volt 0
                           :d1 0
                           :d3 1}))]
         (* d1 d3)))

(def possibilities
  (memoize
   (fn [current-volt adapters]
     (let [possible-next (take-while #(<= (- % current-volt) 3) adapters)]
       (if adapters
         (+ (Math/max 0 (dec (count possible-next)))
            (->> possible-next
                 (map (fn [next-volt] (drop-while (fn [a] (<= a next-volt)) adapters)))
                 (map possibilities possible-next)
                 (reduce +)))
         1)))))

(defn part2 [input]
  (->> input
       parse-input
       sort
       (possibilities 0)
       inc))

(comment
  (part1 input)
  (part2 exercise-input)
  (part2 exercise-input-2)
  (part2 input))
