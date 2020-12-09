(ns aoc-2020.day-9.solution
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as c]))

(def in (->> "resources/day-9.txt"
            slurp
            s/split-lines
            (map #(Long/parseLong %))))


(defn solution [preamble the-rest]
  (let [n (first the-rest)
        sums (map #(apply + %) (c/cartesian-product preamble preamble))
        sum (filter #(= n %) sums)]
    (if (empty? sum)
      n
      (recur (conj preamble n) (drop 1 the-rest)))))

(defn solution-first-part []
  (solution (take 25 in) (drop 25 in)))

(defn solution-2-i [in target len l]
  (let [partitions (partition len (drop l in))
        filtered (filter #(= target (reduce + %)) partitions)
        ans (first filtered)]
    (if (empty? filtered)
      (if (= (inc l) len)
        (recur in target (inc len) 0)
        (recur in target len (inc l)))
      (+ (reduce min ans) (reduce max ans)))))

(defn solution-second-part []
  (solution-2-i in 90433990 2 0))
