(ns advent-2020.day02)

(def exercise-input "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(def input (slurp "resources/day02.txt"))

(defn parse-input [input]
  (->> (re-seq #"(\d+)-(\d+) (\w): (.*)" input)
       (map rest)
       (map (fn [[min max letter password]]
              [(parse-long min)
               (parse-long max)
               letter
               password]))))

(defn part1 [input]
  (->> input
       parse-input
       (filter (fn [[min max letter password]]
                 (<=
                  min
                  (-> (frequencies password) (update-keys str) (get letter ##Inf))
                  max)))
       count))

(defn part2 [input]
  (->> input
       parse-input
       (filter (fn [[position-1 position-2 letter password]]
                 (or
                  (and (= (str (get password (dec position-1))) letter)
                       (not= (str (get password (dec position-2))) letter))
                  (and (= (str (get password (dec position-2))) letter)
                       (not= (str (get password (dec position-1))) letter)))))
       count))

(comment
  (parse-input exercise-input)
  (part1 input)
  (part2 exercise-input)
  (part2 input))
