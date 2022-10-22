(ns advent-2020.day21
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp "resources/day21.txt"))

(defn parse-input [input]
  (map
   (fn [line]
     (let [[ingredients allergens] (str/split line  #"\(contains ")]
       {:ingredients (set (str/split ingredients #" "))
        :allergens (set (str/split (str/replace allergens ")" "") #", "))}))
   (str/split-lines input)))

(defn mappings [from to list]
  (apply hash-map
         (mapcat
          (fn [ingredient]
            [ingredient
             (map to (filter (fn [list-entry] ((get list-entry from) ingredient)) list))])
          (set (mapcat from list)))))

(defn- all->ing [recipes]
  (loop [all->ing {}
         all->ing? (mappings :allergens :ingredients recipes)]
    (if (empty? all->ing?)
      all->ing
      (let [[allergen ingredient] (some
                                   (fn [allergen]
                                     (when (= (count (apply set/intersection (all->ing? allergen))) 1)
                                       [allergen (first (apply set/intersection (all->ing? allergen)))]))
                                   (keys all->ing?))]
        (recur (assoc all->ing allergen ingredient)
               (update-vals (dissoc all->ing? allergen) (fn [ings] (map #(disj % ingredient )ings))))))))

(defn part1 [input]
  (let [recipes (parse-input input)
        ings (mapcat :ingredients recipes)
        all->ing (all->ing recipes)]
    (->> ings
         (remove #((set (vals all->ing)) %))
         count)))

(defn part2 [input]
  (->> input
       parse-input
       all->ing
       (sort-by first)
       (map second)
       (str/join ",")))

(comment
  (part1 input)
  (part2 input))
