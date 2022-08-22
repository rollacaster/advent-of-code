(ns advent-2020.day14
  (:require
    [clojure.string :as str]))

(def exercise-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def input (slurp "resources/day14.txt"))

(defn parse-input [input]
  (->> input
       (re-seq #"(mask = ([X10]+)|mem\[(\d+)\] = (\d+))")
       (mapv (fn [[_ _ mask address value]]
               (cond
                 mask {:type :mask :value mask}
                 :else {:type :write :address (parse-long address) :value (parse-long value)})))))

(defn bitmask [mask value]
  (let [binary-value (str/replace (format (str "%" (count mask) "s") (Integer/toBinaryString value)) " " "0")]
    (Long/parseLong
     (apply str
            (map (fn [v m]
                   (case m
                     \X v
                     \1 "1"
                     \0 "0"))
                 binary-value mask))
     2)))


(defn part1 [input]
  (let [{:keys [values]} (reduce
                          (fn [{:keys [mask] :as state} {:keys [type value] :as op}]
                            (case type
                              :mask (assoc state :mask value)
                              :write (assoc-in state [:values (:address op)] (bitmask mask value))))
                          {:values {}}
                          (parse-input input))]
    (->> values
         vals
         (reduce +))))

(def exercise-input2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(defn bitmask2 [mask address]
  (let [binary-address (str/replace (format (str "%" (count mask) "s") (Integer/toBinaryString address)) " " "0")]
    (loop [addresses [""]
           [address-bit & address] binary-address
           [mask-bit & mask] mask]
      (if (nil? mask-bit)
        (map #(Long/parseLong % 2) addresses)
        (case mask-bit
          \0 (recur (map #(str % address-bit) addresses)
                    address
                    mask)
          \1 (recur (map #(str % "1") addresses)
                    address
                    mask)
          \X (recur (concat
                     (map #(str % "0") addresses)
                     (map #(str % 1) addresses))
                    address
                    mask))))))

(defn part2 [input]
  (let [{:keys [values]} (reduce
                          (fn [{:keys [mask] :as state} {:keys [type value] :as op}]
                            (case type
                              :mask (assoc state :mask value)
                              :write (update state :values (fn [values]
                                                             (let [addresses (bitmask2 mask (:address op))]
                                                               (merge values
                                                                      (apply hash-map
                                                                             (interleave addresses
                                                                                         (repeat value)))))))))
                          {:values {}}
                          (parse-input input))]
    (->> values
         vals
         (reduce +))))

(comment
  (part1 exercise-input)
  (part1 input)
  (part2 exercise-input2)
  (part2 input))
