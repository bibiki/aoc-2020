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
