(ns aoc-2020.day-11.solution
  (:require [clojure.string :as s]))

(def in (->> "resources/day-11.txt"
            slurp
            s/split-lines
            (map #(vec (seq %)))
            vec))

(defn- get-upper [x y layout]
  (if (= 0 x)
    '()
    (let [row (layout (dec x))
          l (if (> y 0) (row (dec y)))
          m (row y)
          r (if (< y (dec (count (first layout)))) (row (inc y)))]
      (filter #(not= nil %) (list l m r)))))

(defn- get-lower [x y layout]
  (if (= (dec (count layout)) x)
    '()
    (let [row (layout (inc x))
          l (if (> y 0) (row (dec y)))
          m (row y)
          r (if (< y (dec (count (first layout)))) (row (inc y)))]
      (filter #(not= nil %) (list l m r)))))

(defn- get-left [x y layout]
  (if (= 0 y)
    '()
    (list ((layout x) (dec y)))))

(defn- get-right [x y layout]
  (if (>= y (dec (count (first layout))))
    '()
    (list ((layout x) (inc y)))))

(defn get-neighbors [x y layout]
  (reduce concat (map #(% x y layout) [get-upper get-lower get-left get-right])))

(defn get-next-value [x y layout]
  (if (= \. ((layout x) y))
    \.
    (let [seat ((layout x) y)
          all-neighbors (get-neighbors x y layout)
          empty-neighbors (count (filter #(= \L %) all-neighbors))
          taken-neighbors (count (filter #(= \# %) all-neighbors))]
      (cond
        (and (= 0 taken-neighbors) (= seat \L)) \#
        (and (<= 4 taken-neighbors) (= seat \#)) \L
        :else seat))))

(defn find-next [layout get-next-value]
  (let [rs (count layout)
        cs (count (first layout))
        all (for [x (range rs) y (range cs)] (get-next-value x y layout))]
    (vec (map vec (partition (count (first layout)) all)))))

(defn find-layout [get-next-value last current]
  (if (= current last)
    current
    (recur get-next-value current (find-next current get-next-value))))

(defn count-taken-seats [row]
  (count (filter #(= \# %) row)))

(defn any-taken [layout a b dir]
  (let [aa (+ a (dir 0))
        bb (+ b (dir 1))
        rs (count layout)
        cs (count (first layout))]
    (if (or (< aa 0) (= aa rs) (< bb 0) (= bb cs))
      false
      (let [p ((layout aa) bb)]
        (cond
          (= \L p) false
          (= \# p) true
          :else (recur layout aa bb dir))))))

(defn get-next-value-i [x y layout]
  (let [rows (count layout)
        cols (count (first layout))
        seat ((layout x) y)
        e (if (any-taken layout x y [0 1]) 1 0)
        w (if (any-taken layout x y [0 -1]) 1 0)
        n (if (any-taken layout x y [-1 0]) 1 0)
        s (if (any-taken layout x y [1 0]) 1 0)
        ne (if (any-taken layout x y [-1 1]) 1 0)
        nw (if (any-taken layout x y [-1 -1]) 1 0)
        se (if (any-taken layout x y [1 1]) 1 0)
        sw (if (any-taken layout x y [1 -1]) 1 0)
        neighbors (+ e w s n ne nw se sw)]
    (cond
      (and (= 0 neighbors) (= seat \L)) \#
      (and (<= 5 neighbors) (= seat \#)) \L
      :else seat)))

(defn solution-first-part []
  (->> in
       (find-layout get-next-value [])
       (map count-taken-seats)
       (reduce +)))

(defn solution-second-part []
  (->> in
       (find-layout get-next-value-i [])
       (map count-taken-seats)
       (reduce +)))
