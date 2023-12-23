(ns advent-of-code.day05
  (:require [clojure.string :as str]))

(def input (slurp "resources/day05.txt"))

(defn parse [input]
  (let [[seeds & mappings] (str/split input #"\n\n")]
    {:seeds (mapv parse-long (re-seq #"\d+" seeds))
     :mappings
     (mapv
      (fn [mapping]
        (let [[description & section] (str/split mapping #"\n")
              [_ from to](re-find #"(\w+)\-to\-(\w+)\smap:" description)]
          {:from from
           :to to
           :sections
           (mapv
            (fn [line]
              (mapv parse-long (re-seq #"\d+" line)))
            section)}))
      mappings)}))

(defn part1 [input]
  (let [{:keys [seeds mappings]} (parse input)]
    (->> seeds
         (map
          (fn [seed]
            (reduce
             (fn [seed {:keys [sections]}]
               (or (some
                    (fn [[destination source lenght]]
                      (when (<= source seed (+ source lenght))
                        (+ destination (Math/abs (- source seed)))))
                    sections)
                   seed))
             seed
             mappings)))
         (apply min))))

(defn apply-mapping [seed mapping]
  (loop [res []
         seed seed
         mapping mapping]
    (let [[{:keys [from to]} & mappings] mapping
          [mstart mend] from
          [start end] seed
          [tstart] to]
      (if (or (nil? mapping) (empty? seed))
        (concat res (if (not-empty seed) [seed] seed))
        (cond
          (<= mstart start mend)
          (recur (conj res [(+ start (- tstart mstart))
                            (+ (min mend end) (- tstart mstart))])
                 (if (> end mend) [(inc mend) end] [])
                 mappings)

          (<= start mstart end)
          (recur (conj res [(+ mstart (- tstart mstart))
                            (+ (min end mend) (- tstart mstart))])
                 (if (< start mstart) [start (dec mstart)] [])
                 mappings)

          :else
          (recur res seed mappings))))))

(defn part2 [input]
  (let [{:keys [seeds mappings]}
        (-> (parse input)
            (update :seeds (fn [seeds]
                             (->> (partition 2 seeds)
                                  (mapv (fn [[start length]] [start (+ start length)])))))
            (update :mappings (fn [mappings]
                                (mapv
                                 (fn [mapping]
                                   (mapv
                                    (fn [[dest start length]]
                                      {:from [start (+ start length)] :to  [dest (+ dest length)]})
                                    (:sections mapping)))
                                 mappings))))]
    (->> seeds
         (mapcat
          (fn [seed]
            (reduce
             (fn [seeds mapping]
               (mapcat
                (fn [seed] (apply-mapping seed mapping))
                seeds))
             [seed]
             mappings)))
         (map first)
         (sort <)
         first)))

(comment
  (part1 input)
  ;; => 346433842
  (part2 input)
  ;; => 60294664
  )
