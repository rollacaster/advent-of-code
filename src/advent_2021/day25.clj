(ns advent-2021.day25
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-input [input]
  {:cucumbers
   (apply
    merge
    (for [[y line] (map-indexed vector (str/split-lines input))
          [x cell] (map-indexed vector line)
          :when (not= cell \.)]
      {[x y] (if (= cell \>) :east :south)}))
   :width (count (first (str/split-lines input)))
   :height (count (str/split-lines input))})

(defn move-east [cucumbers field-width]
  (apply merge
         (map (fn [[[x y] cucumber]]
                (if (and (= cucumber :east) (nil? (get cucumbers [(mod (inc x) field-width) y])))
                  {[(mod (inc x) field-width) y] cucumber}
                  {[x y] cucumber}))
              cucumbers)))

(defn move-south [cucumbers field-height]
  (apply merge
         (map (fn [[[x y] cucumber]]
                (if (and (= cucumber :south) (nil? (get cucumbers [x (mod (inc y) field-height)])))
                  {[x (mod (inc y) field-height)] cucumber}
                  {[x y] cucumber}))
              cucumbers)))

(defn part1 [input]
  (loop [{:keys [cucumbers width height] :as inp} (parse-input input)
         step 0]
    (let [moved-cucumbers (-> cucumbers
                              (move-east width)
                              (move-south height))]
      (if (= cucumbers moved-cucumbers)
        (inc step)
        (recur (assoc inp :cucumbers moved-cucumbers) (inc step))))))

(comment
  (part1 (slurp (io/resource "day25.txt"))))
