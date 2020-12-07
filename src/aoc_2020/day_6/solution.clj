(ns aoc-2020.day-6.solution
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(def groups (s/split (slurp "resources/day-6.txt") #"\n\n"))

(def group-answers (map #(s/replace % "\n" "") groups))

(def counts (map #(count (dedupe (seq %))) group-answers))

(def group-answers-2 (map #(s/split % #"\n") groups))

(defn answer-sets [ans]
  (map #(set (seq %)) ans))

(def group-answers-sets (map answer-sets group-answers-2))

(def intersected (map #(reduce sets/intersection %) group-answers-sets))

(defn first-part []
  (reduce + (map #(count (distinct (seq %))) group-answers)))

(defn second-part []
  (reduce + (map #(count %) intersected)))

(println (first-part) (second-part))
