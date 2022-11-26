(ns advent-2021.temp
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]))

(defn tick
  [state]
  (assoc state :8 (:0 state)
               :7 (:8 state)
               :6 (+ (:0 state) (:7 state))
               :5 (:6 state)
               :4 (:5 state)
               :3 (:4 state)
               :2 (:3 state)
               :1 (:2 state)
               :0 (:1 state)))

(defn total-sum-of-fish
  [state]
  (reduce-kv #(+ %1 %3) 0 state))

(defn update-state
  [state fish]
  (let [key (keyword (str fish))]
    (assoc state key (inc (get state key)))))

(def initial-state (zipmap (map (comp keyword str) (range 0 9)) (repeat 0)))

(defn run-simulation
  [fish days]
  (->> fish
    (reduce update-state initial-state)
    (iterate tick)
    (take (inc days))
    last
    total-sum-of-fish))

(comment
  (with-open [rdr (io/reader (io/resource "day6.txt"))]
    (let [fish (->> (line-seq rdr)
                 (map #(str/split % #","))
                 (map (fn [t] (map #(Integer/parseInt %) t)))
                 flatten)]

      (println "Total number of fish after 80 iterations:"
        (run-simulation fish 80))

      (println "Total number of fish after 256 iterations:"
        (run-simulation fish 256))))
  )

