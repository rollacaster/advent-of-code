(ns advent-2020.day25)

(defn loop-size [public-key]
  (loop [subject-number 7
         n 1]
    (if (= subject-number public-key)
      n
      (recur (rem (* 7 subject-number) 20201227)
             (inc n)))))

(defn encryption-key [loop-size subject-number]
  (loop [curr 1
         n loop-size]
    (if (= n 0)
      curr
      (recur (rem (* curr subject-number) 20201227)
             (dec n)))))

(defn part1 [public-key1 public-key2]
  (encryption-key (loop-size public-key1) public-key2))

(comment
  (part1 9093927 11001876))
