(ns aoc-2020.day-3.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "resources/day-3.txt")))

(defn is-tree? [row index]
  (= \# (first (drop index (seq row)))))

(defn- calc-x-index [row-index row-string x-slope]
  "to prevent array index out of bounds"
  (rem (* row-index x-slope) (count row-string)))

(defn solution [x-slope input]
  "solution to the first part of the problem"
  (let [slope-xs (map-indexed #(calc-x-index %1 %2 x-slope) input)
        tree-or-not (map #(is-tree? %1 %2) input slope-xs)
        only-trees (filter #(= true %) tree-or-not)]
    (count only-trees)))

(defn solution-second-part [input]
  (let [with-slope-y-1 (map #(solution % input) '(1 3 5 7))
        empty-odd-rows (map-indexed #(if (= 0 (rem %1 2)) %2 "") input)
        remove-empty-rows (filter #(not= % "") empty-odd-rows)
        with-slope-y-2 (solution 1 remove-empty-rows)]
    (* with-slope-y-2 (reduce * with-slope-y-1))))
