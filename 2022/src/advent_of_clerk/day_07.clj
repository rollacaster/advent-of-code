;; # 🎄 Advent of Clerk: Day 7
(ns advent-of-clerk.day-07
  (:require
   [clojure.string :as str]
   [clojure.zip :as z]))

(def input (slurp "resources/day07.txt"))

(defn- cd [tree output]
  (let [[_ dir] (re-find #"\$ cd (.*)" output)]
    (cond
      (= dir "/") tree
      (= dir "..") (z/up tree)
      :else (loop [tree (z/down tree)]
              (if (= (:name (z/node tree)) dir)
                tree
                (recur (z/right tree)))))))

(defn- size [tree]
  (reduce
   (fn [result child]
     (+ result (or (:size child) (size child))))
   0
   (:children tree)))

(defn- add-dir [output tree]
  (let [[_ name] (re-find #"dir (\w+)" output)]
    (z/append-child tree {:type :dir :name name :children []})))

(defn- add-file [output tree]
  (let [[_ size name] (re-find #"(\d+) (\w+)" output)]
    (z/append-child tree {:type :file
                          :size (parse-long size)
                          :name name})))

(defn file-tree [input]
  (z/root
   (reduce
    (fn [tree output]
      (cond
        (and (str/starts-with? output "$") (str/includes? output "ls")) tree
        (and (str/starts-with? output "$") (str/includes? output "cd")) (cd tree output)
        (str/starts-with? output "dir") (add-dir output tree)
        :else (add-file output tree)))
    (z/zipper :children :children (fn [node children] (assoc node :children children)) {:name "/" :children []})
    (str/split-lines input))))

(defn part1 [input]
  (->> (file-tree input)
       (tree-seq :children :children)
       (filter #(and (= (:type %) :dir) (<= (size %) 100000)))
       (map size)
       (reduce +)))

(defn part2 [input]
  (let [tree (file-tree input)
        used-space (size tree)
        needed-space 30000000
        available-space 70000000]
    (->> (file-tree input)
         (tree-seq :children :children)
         (map size)
         (sort <)
         (drop-while #(< (+ (- available-space used-space) %) needed-space))
         first)))

(part1 input)

(part2 input)
