(ns aoc-2020.day-16.solution
  (:require [clojure.string :as s]))

(def in (-> "resources/day-16.txt" slurp s/split-lines))

(def rules (take 20 in))

(def ticket (first (drop 22 in)))

(def nearby-tickets-str (drop 25 in))

(defn str->num [ticket]
  (map #(Integer/parseInt %) ticket))

(def nearby-tickets (map str->num (map #(s/split % #",") nearby-tickets-str)))

(defn get-rule-ranges [rule]
  (let [without-name (s/split rule #": ")
        ranges (s/split (without-name 1) #" or ")
        one (map #(Integer/parseInt %) (s/split (ranges 0) #"-"))
        two (map #(Integer/parseInt %) (s/split (ranges 1) #"-"))]
    (vector (vec one) (vec two))))

(def rule-ranges (map get-rule-ranges rules))

(defn is-valid? [ranges num]
  (let [f (ranges 0)
        sec (ranges 1)]
    (or (<= (f 0) num (f 1)) (<= (sec 0) num (sec 1)))))

(defn validate-one-num [num ranges-list]
  (or (reduce #(or %1 %2) false (map #(is-valid? % num) ranges-list)) num))

(defn validate-nums [nums ranges-list]
  (map #(validate-one-num % ranges-list) nums))

(defn solution [rules tickets]
  (->> tickets
      (map #(validate-nums % rules))
      (map #(filter number? %))
      (filter #(not-empty %))
      (map #(first %))
      (reduce +)))


(defn rotate [tickets]
  (map flatten (reduce #(if (nil? %1) %2 (map list %1 %2)) tickets)))

(defn is-ticket-valid? [ticket rules]
  (let [t (validate-nums ticket rules)
        tt (filter number? t)]
    (empty? tt)))

(def tickets-by-field (->> nearby-tickets
                           (filter #(is-ticket-valid? % rule-ranges))
                           rotate))

(defn is-rule-valid-for-all [nums rule]
  (let [a (map #(is-valid? rule %) nums)]
    (reduce #(and %1 %2) true a)))

(defn mapper [field rules]
  (filter #(is-rule-valid-for-all field %) rules))

(defn solution-2 [tickets-rotated rules res]
  "find rules that validate nth field for all tickets"
  "find the n for which there is only one such rule"
  "that rule validates that n. remove that rule, and recur"
  (if (empty? rules)
    res
    (let [grid (map #(mapper % rules) tickets-rotated)
          grid-indexed (map-indexed #(vector %1 %2) grid)
          f (filter #(= 1 (count (% 1))) grid-indexed)
          r (first ((first f) 1))
          next-rules (filter #(not= r %) rules)]
      (recur tickets-rotated next-rules (cons (first f) res)))))

