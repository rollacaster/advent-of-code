(ns advent-2020.day04
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def exercise-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def input (slurp "resources/day04.txt"))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map #(str/replace % #"\n" " "))
       (mapv #(mapv (fn [attr-str] (str/split attr-str #":"))(str/split % #" ")))))


(def required
  #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"} )

(defn part1 [input]
  (let [data (parse-input input)]
    (->> data
         (filter (fn [passport] (empty? (set/difference required (set (map first passport))))))
         count)))

(def rules
  {"byr" (fn [v] (<= 1920 (parse-long v) 2002))
   "iyr" (fn [v] (<= 2010 (parse-long v) 2020))
   "eyr" (fn [v] (<= 2020 (parse-long v) 2030))
   "hgt" (fn [v] (when-let [[_ height type] (re-find #"(\d+)(cm|in)$" v)]
                  (case type
                    "cm" (<= 150 (parse-long height) 193)
                    "in" (<= 59 (parse-long height) 76))))
   "hcl" (fn [v] (re-matches #"#[0-9a-f]{6}" v))
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" (fn [v] (re-matches #"\d{9}" v))})

(def invalid "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def valid "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(defn part2 [input]
  (let [data (parse-input input)]
    (->> data
         (filter (fn [passport] (empty? (set/difference required (set (map first passport))))))
         (map #(apply hash-map (flatten %)))
         (filter (fn [passport] (every? (fn [[attr rule]] (rule (get passport attr))) rules)))
         count)))


(comment
  (part1 exercise-input)
  (part1 input)
  (part2 invalid)
  (part2 valid)
  (part2 input))
