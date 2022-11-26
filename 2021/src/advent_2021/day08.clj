(ns advent-2021.day08
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(def segment-counts
  {0 6
   1 2
   2 5
   3 5
   4 4
   5 5
   6 6
   7 3
   8 7
   9 6})

(def counts-segment
  {2 1
   5 #{2 3 5}
   4 4
   6 #{0 6 9}
   3 7
   7 8})

(def input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(re-seq #"(\w*)\s|\s(\w*)"input)
(defn prep-input [input]
  (for [line (str/split-lines input)]
    (for [line-parts (str/split line  #" \| ")]
      (str/split line-parts #" "))))

(defn part1 [input]
  (->> [1 4 7 8]
       (map
        (fn [number]
          (get
           (->> input
                prep-input
                (mapcat second)
                (map count)
                frequencies)
           (segment-counts number)
           0)))
       (apply +)))

(defn deduce [input]
  (let [number-stats
        (->> input
             (map #(hash-map :signal %
                             :count (count %)
                             :number (counts-segment (count %))))
             (group-by :number)
             (map (fn [[number number-desc]]
                    {number
                     {:descs number-desc
                      :frequencies
                      (frequencies  (apply str  (map :signal number-desc)))}}))
             (apply merge))
        hat (first
             (set/difference
              (set (keys (:frequencies  (get number-stats 7))))
              (set (keys (:frequencies  (get number-stats 1))))))
        [third sixth] (->> (:frequencies (get number-stats #{0 6 9}))
                           (merge-with
                            +
                            (:frequencies  (get number-stats 1)))
                           (filter
                            (fn [[k]]
                              ((set (keys (:frequencies  (get number-stats 1)))) k)))
                           (sort-by second)
                           keys)
        second (first
                (some
                 #(when (= (second %) 2) %)
                 (merge-with
                  +
                  (:frequencies  (get number-stats 4))
                  (:frequencies  (get number-stats #{3 2 5})))))
        fourth (first
                (set/difference
                 (set (keys (:frequencies  (get number-stats 4))))
                 (set [second third sixth])))
        fifth (first
               (set/difference
                (->> (:frequencies (get number-stats #{3 2 5}))
                     (merge-with + (:frequencies  (get number-stats 8)))
                     (filter (fn [[_ k]] (= k 2)))
                     keys
                     set)
                (set [second])))]
    [hat
     second
     third
     fourth
     fifth
     sixth
     (first
      (set/difference
       (set (keys (:frequencies  (get number-stats 8))))
       (set [hat second third fourth fifth sixth])))]))

(def transform
  {0 [1 2 3 5 6 7]
   1 [3 6]
   2 [1 3 4 5 7]
   3 [1 3 4 6 7]
   4 [2 3 4 6]
   5 [1 2 4 6 7]
   6 [1 2 4 5 6 7]
   7 [1 3 6]
   8 [1 2 3 4 5 6 7]
   9 [1 2 3 4 6 7]})

(defn part2 [input]
  (->> input
       prep-input
       (map
        (fn [input]
          (let [[signals numbers] input
                t (apply hash-map (mapcat
                                   (fn [[n s]]
                                     [(set
                                       (map
                                        #(nth (deduce signals) (dec %))
                                        s))
                                      n])
                                   transform))]
            (Integer/parseInt (apply str (map (comp t set) numbers))))))
       (apply +)))

(comment
  (part1 (slurp (io/resource "day8.txt")))
  (part2 (slurp (io/resource "day8.txt"))))
