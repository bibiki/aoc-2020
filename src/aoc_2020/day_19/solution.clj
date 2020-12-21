(ns aoc-2020.day-19.solution
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(def input (-> "resources/day-19.txt" slurp s/split-lines))

(def rules (->> input
                (take-while #(not= "" %))
                (map #(s/split % #": "))
                (map #(vector (% 0) (s/split (% 1) #" ")))
                (into {})))

(def strs (->> input (drop-while #(not= "" %)) rest))

(defn has-no-numbers? [rule]
  (let [a (reduce str "" rule)]
    (every? #(contains? #{\a \b \| \( \)} %) a)))

(defn build-regexp [rules start]
  (if (has-no-numbers? start)
    (reduce str "^" start)
    (let [f (first (drop-while #(has-no-numbers? %) start))
          rule (rules f)
          rule-s (if (contains? (set rule) "|") (concat (list "(") rule (list ")")) rule)]
      (recur rules (concat (take-while #(not= f %) start) rule-s (drop 1 (drop-while #(not= f %) start)))))))

(defn solution-first-part []
  (let [regexp (build-regexp rules '("8" "11"))
        matches (map #(.matches % regexp) strs)]
    (count (filter #(= true %) matches))))

(defn m [p1 p2 max-depth s]
  "matches strings that start with at least max-depth occurences of p1,"
  "and follow with max-depth occurences of p2"
  (if (<= max-depth 0)
    false
    (let [r (str p1 "{" (inc max-depth) ",}" (.substring p2 1) "{" max-depth "}")
          t (.matches s r)]
      (if t t (recur p1 p2 (dec max-depth) s)))))

(defn solution-second-part []
  (let [fourty-two (build-regexp rules '("42"))
        thirty-one (build-regexp rules '("31"))
        max-depth 4
        regexp (str (.substring fourty-two 1) "+" (.substring thirty-one 1) "+")
        matches (map #(m fourty-two thirty-one max-depth %) strs)]
    (count (filter #(= true %) matches))))
