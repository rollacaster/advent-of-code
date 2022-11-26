(ns advent-2021.day20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-input [input]
  (let [[enhancement-str input-str] (str/split input #"\n\n")]
    {:enhancement-algorithm (str/replace (str/join enhancement-str) "\n" "")
     :input-image (str/split-lines input-str)}))

(defn neighbours [coord round input-image]
  (map
   #(get-in input-image (reverse (map + coord %)) (if (even? round) "." "#"))
   [[-1 -1] [0 -1] [1 -1]
    [-1  0] [0  0] [1 0]
    [-1 1] [0 1] [1 1]]))

(defn read-bin [n] (read-string (str "2r" n)))

(defn enhance-index [neighbours]
  (read-bin (apply str (map #(case % \. 0 \# 1) (str/join neighbours)))))

(defn get-pixel [idx enhancement-algorithm]
  (nth enhancement-algorithm idx))

(defn enlarge-image [round input-image]
  (vec
   (concat
    [(apply str (repeat (+ 2 (count input-image)) (if (even? round) "." "#")))]
    (map (fn [line] (str (if (even? round) "." "#") line (if (even? round) "." "#"))) input-image)
    [(apply str (repeat (+ 2 (count input-image)) (if (even? round) "." "#")))])))

(defn enhance-image [enhancement-algorithm input-image round]
  (let [input-image (enlarge-image round input-image)]
    (vec
     (for [y (range 0 (count input-image))]
       (apply str
              (for [x (range 0 (count (first input-image)))]
                (-> [x y]
                    (neighbours round input-image)
                    (enhance-index)
                    (get-pixel enhancement-algorithm))))))))

(defn count-light-pixel [image]
  (count (filter #(= % \#) (str/join image))))

(defn part1 [input]
  (count-light-pixel
   (let [max-round 2
         {:keys [enhancement-algorithm input-image]} (parse-input input)]
     (loop [round 0
            input-image input-image]
       (if (= round max-round)
         input-image
         (recur
          (inc round)
          (enhance-image enhancement-algorithm input-image round)))))))

(defn part2 [input]
  (count-light-pixel
   (let [max-round 50
         {:keys [enhancement-algorithm input-image]} (parse-input input)]
     (loop [round 0
            input-image input-image]
       (if (= round max-round)
         input-image
         (recur
          (inc round)
          (enhance-image enhancement-algorithm input-image round)))))))

(comment
  (part1 (slurp (io/resource "day20.txt")))
  (part2 (slurp (io/resource "day20.txt"))))
