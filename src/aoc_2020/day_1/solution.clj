(ns aoc-2020.day-1.solution
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as s]
            [aoc-2020.core :as core]))

(defn solution [lists filterer]
  (let [cp (apply c/cartesian-product lists)
        target (filter filterer cp)]
    (apply * (first target))))

(defn filterer [tuple]
  (= 2020 (apply + tuple)))

(defn solution-first-part []
  (let [l (core/get-list-of-numbers "resources/day-1.txt")]
    (solution [l l] filterer)))

(defn solution-second-part []
  (let [l (core/get-list-of-numbers "resources/day-1.txt")]
    (solution [l l l] filterer)))
