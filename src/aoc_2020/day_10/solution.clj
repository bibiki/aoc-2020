(ns aoc-2020.day-10.solution
  (:require [clojure.string :as s]))

(def in (->> "resources/day-10.txt"
           slurp
           s/split-lines
           (map #(Integer/parseInt %))
           sort))

(defn solution [in-sorted diff-one diff-three]
  (if (empty? in-sorted)
    (* diff-one diff-three)
    (let [first-two (take 2 in-sorted)]
      (if (= 1 (- (last first-two) (first first-two)))
        (recur (rest in-sorted) (inc diff-one) diff-three)
        (recur (rest in-sorted) diff-one (inc diff-three))))))

(defn solution-first-part []
  (-> in
      (conj 0)
      (solution 0 0)))

(defn find-continuous [in-sorted acc current]
  (if (empty? in-sorted)
    (conj acc current)
    (let [pairs (take 2 in-sorted)
          a (first pairs)
          b (last pairs)]
      (if (= 1 (- b a))
        (recur (drop 1 in-sorted) acc (conj current a))
        (recur (drop 1 in-sorted) (conj acc (conj current a)) '())))))

(defn mapper [i]
  (let [c (count i)]
    (cond
      (= 5 c) 7
      (= 4 c) 4
      (= 3 c) 2
      :else 1)))

(defn solution-second-part []
  (-> in
      sort
      (conj 0)
      (find-continuous '() '())
      ((fn [x] (map mapper x)))
      ((fn [x] (reduce * x)))))
