(ns advent-2020.day08)

(def exercise-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def input (slurp "resources/day08.txt"))

(defn parse-input [input]
  (->> input
       (re-seq #"(\w{3}) ([+-]\d+)")
       (mapv (fn [[_ op argument]] [op (read-string argument)]))))

(defn run [program]
  (loop [{:keys [position accumlator processed-instructions] :as state}
         {:processed-instructions []
          :accumlator 0
          :position 0}]
    (let [[op arg] (get program position)]
      (cond
        (= position (count program))
        {:accumlator accumlator
         :state :terminated}
        (=
         (set processed-instructions)
         (set (conj processed-instructions position)))
        {:accumlator accumlator
         :state :non-terminated}
        :else (recur
         (case op
           "nop" (-> state
                     (update :position inc)
                     (update :processed-instructions conj position))
           "jmp" (-> state
                     (update :position + arg)
                     (update :processed-instructions conj position))
           "acc" (-> state
                     (update :accumlator + arg)
                     (update :position inc)
                     (update :processed-instructions conj position))))))))

(defn part1 [input]
  (-> input
      parse-input
      run
      :accumlator))

(defn part2 [input]
  (let [input (parse-input input)]
    (->> input
         (keep-indexed (fn [idx [op]] (when (or (= op "nop") (= op "jmp")) idx)))
         (map (fn [idx] (-> input
                           (update-in [idx 0] {"nop" "jmp" "jmp" "nop"})
                           run)))
         (filter (fn [{:keys [state]}] (= state :terminated)))
         first
         :accumlator)))

(comment
  (part1 exercise-input)
  (part1 input)
  (part2 exercise-input)
  (part2 input))
