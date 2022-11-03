(ns advent-2020.day23)

(def input "315679824")

(defn cup [n c]
  (-> n dec (mod c) inc))

(defn- parse-input [input]
  (->> input
       (re-seq #"\d")
       (mapv parse-long)))

(defn- neighbour->cup [input-cups]
  (let [neighbour-pairs (partition 2 1 input-cups)
        start (ffirst neighbour-pairs)
        end (last (last neighbour-pairs))]
    (assoc
     (reduce
      (fn [neihbour->cup [neihbour cup]]
        (assoc neihbour->cup neihbour cup))
      {}
      neighbour-pairs)
     end start)))

(defn combine [neighbour->cup]
  (loop [state neighbour->cup
         res []]
    (cond
      (and (empty? res) (seq state))
      (recur
       (dissoc state (ffirst state))
       (first state))
      (seq state)
      (recur
       (dissoc state (last res))
       (conj res (state (last res))))
      :else
      res)))

(defn rotate-while [pred coll]
  (let [head (drop-while pred coll)]
    (take (count coll) (concat head coll))))

(defn run [n neighbour->cup]
  (loop [state neighbour->cup
         cur 3
         n n]
    (if (zero? n)
      state
      (let [pick-a (state cur)
            pick-b (state pick-a)
            pick-c (state pick-b)
            next-cur (state pick-c)
            dest (loop [d (cup (dec cur) (count state))]
                   (if ((set [pick-a pick-b pick-c]) d)
                     (recur (cup (dec d) (count state)))
                     d))
            dest-next (state dest)
            next-state (assoc state cur next-cur dest pick-a pick-c dest-next)]
        (recur next-state next-cur (dec n))))))

(defn part1 [input]
  (let [cup-order (combine (run 100 (neighbour->cup (parse-input input))))]
    (->> cup-order
         (rotate-while #(not= % 1))
         dedupe
         (drop 1)
         (apply str))))

(defn part2 [input]
  (let [res (run 10000000
              (neighbour->cup (vec (concat (parse-input input)
                                           (range 10 1000001)))))]
    (* (get res 1) (get res (get res 1)))))

(comment
  (part1 input)
  (part2 input))
