(ns aoc-2020.day-17.solution
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def input (-> "resources/day-17.txt" slurp))

(def space (into {} (for [x (range 27) y (range 27) z (range 27)]
                      [[(- x 13) (- y 13) (- z 13)] 0])))

(def neighbors (filter #(not= [0 0 0] %)
                       (for [ x (range 3) y (range 3) z (range 3)]
                         [(dec x) (dec y) (dec z)])))

(defn seqs->vec [seqs row rez]
  (if (empty? seqs)
    (reduce concat rez)
    (let [f (first seqs)
          ff (map-indexed #(vector (vector 0 row %1) (if (= \# %2) 1 0)) f)]
      (recur (rest seqs) (inc row) (conj rez ff)))))

(defn init-space [space input]
  (let [i (seqs->vec (map seq (s/split-lines input)) 0 [])]
    (into space i)))

(defn neighbor-key [item direction]
  (mapv + item direction))

(defn mapper [space item neighbors]
  (let [item-key (item 0)
        active (item 1)
        neighbor-values (map #(space (neighbor-key item-key %)) neighbors)
        active-neighbors (reduce + (filter #(not= nil %) neighbor-values))]
    (cond
      (and (= 1 active) (<= 2 active-neighbors 3)) [item-key 1]
      (and (= 0 active) (= 3 active-neighbors)) [item-key 1]
      :else [item-key 0])))

(defn solution [space cycle neighbors]
  (if (= 0 cycle)
    space
    (let [next-space (map #(mapper space % neighbors) space)]
      (recur (into {} next-space) (dec cycle) neighbors))))

(defn solution-first-part []
  (let [i (init-space space input)
        c (solution i 6 neighbors)
        a (map #(% 1) c)]
    (reduce + a)))

(def four-d-space (for [x (range 27) y (range 27) z (range 27) w (range 27)] [[(- x 13) (- y 13) (- z 13) (- w 13)] 0]))

(def four-d-neighbors (filter #(not= [0 0 0 0] %) (for [x (range 3) y (range 3) z (range 3) w (range 3)] [(dec x) (dec y) (dec z) (dec w)])))

(defn solution-second-part []
  (let [starting (seqs->vec (map seq (s/split-lines input)) 0 [])
        starting-four-d (map #(vector (conj (% 0) 0) (% 1)) starting)
        temp (into {} four-d-space)
        space (into temp starting-four-d)
        c (solution space 6 four-d-neighbors)
        a (map #(% 1) c)]
    (reduce + a)))
