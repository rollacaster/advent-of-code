(ns advent-2020.day19
  (:require [clojure.string :as str]))

(def input (slurp "resources/day19.txt"))

(defn parse-input [input]
  (let [[rules tests] (str/split input #"\n\n")]
       {:rules
        (into {}
              (map
               (fn [[_ rule content]]
                 (hash-map
                  (parse-long rule)
                  (cond
                    (str/includes? content "\"") {:type :literal :value (str/replace content "\"" "")}
                    (str/includes? content "|") {:type :or :value (map (comp #(map parse-long %) #(str/split % #" ") str/trim)  (str/split content #"\|"))}
                    :else {:type :rule :value (map parse-long (str/split content #" "))})))
               (re-seq #"(\d+): (.*)" rules)))
        :tests (str/split-lines tests)}))

(defn combine [colls]
  (if (empty? colls)
    '(())
    (for [more (combine (rest colls))
          x (first colls)]
      (str x (apply str more)))))

(defn step [rule rules]
  (let [{:keys [type value]} rule]
    (case type
      :literal [value]
      :rule (combine (map #(step (get rules %) rules) value))
      :or (mapcat
           (fn [or-rule] (step {:type :rule :value or-rule} rules))
           value))))

(defn part1 [input]
  (let [{:keys [rules tests]} (parse-input input)
        valid (set (step (get rules 0) rules))]
    (count (filter (fn [test] (valid test)) tests))))

(defn pattern [rule-42 rule-31 n]
  (re-pattern
   (str (str "(" (str/join "|"rule-42) ")+")
        (apply str (repeat n (str "(" (str/join "|"rule-42) ")")))
        (apply str (repeat n (str "(" (str/join "|"rule-31) ")"))))))

(defn part2 [input]
  (let [{:keys [rules tests]} (parse-input input)
        rule-42 (set (step (get rules 42) rules))
        rule-31 (set (step (get rules 31) rules))]
    (count (filter (fn [test] (some #(re-matches (pattern rule-42 rule-31 %) test) (range 1 6))) tests))))

(comment
  (part1 input) ;; => 210
  (part2 input) ;; => 422
)
