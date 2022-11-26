(ns advent-2021.day12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn prep-input [input]
  (map #(str/split % #"-") (str/split-lines input)))

(defn all-routes [input]
  (apply merge
         (->> (prep-input input)
              (mapcat (fn [[a b]] [[a b] [b a]]))
              (group-by first)
              (map (fn [[from tos]] (hash-map from (set (->> tos
                                                            (map second)
                                                            (remove #(= % "start"))))))))))

(defn lower-case? [s] (every? #(Character/isLowerCase %) s))

(defn find-routes [routes found-routes route]
  (doseq [node (routes (last route))]
    (let [new-route (conj route node)]
      (cond
        (= node "end") (swap! found-routes conj new-route)
        (and (lower-case? node) ((set route) node)) nil
        :else (find-routes routes found-routes new-route)))))

(defn part1 [input]
  (let [found-routes (atom [])]
    (find-routes (all-routes input) found-routes ["start"])
    (count @found-routes)))

(def invalid-route?
  (memoize (fn [node route]
             (and (lower-case? node)
                  (let [frequs (vals (frequencies (filter lower-case? route)))]
                    (cond
                      (some #(> % 2) frequs) true
                      (and (get (frequencies frequs) 2) (>= (get (frequencies frequs) 2) 2))
                      true))))))

(defn find-routes-2 [routes found-routes route]
  (doseq [node (routes (last route))]
    (let [new-route (conj route node)]
      (cond
        (invalid-route? node route) nil
        (= node "end") (swap! found-routes conj new-route)
        :else (find-routes-2 routes found-routes new-route)))))

(defn part2 [input]
  (let [found-routes (atom [])]
    (find-routes-2 (all-routes input) found-routes ["start"])
    (count @found-routes)))

(comment
  (part1 (slurp (io/resource "day12.txt")))
  (part2 (slurp (io/resource "day12.txt"))))

