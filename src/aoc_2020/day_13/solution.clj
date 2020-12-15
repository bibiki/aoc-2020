(ns aoc-2020.day-13.solution
  (:require [clojure.string :as s]))

(def arrival 1006401)

(def bus-strings (s/split "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,449,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,19,x,x,x,x,x,x,x,x,x,x,x,607,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29" #","))

(def bus-numbers (map #(Integer/parseInt %) (filter #(not= "x" %) bus-strings)))


(defn solution [arrival bus-lines wait]
  (let [line (filter #(= 0 (rem (+ wait arrival) %)) bus-lines)]
    (if (= 1 (count line))
      (* (first line) wait)
      (recur arrival bus-lines (inc wait)))))

(defn solution-first-part []
  (solution arrival bus-numbers 1))

(defn find-first [m r x step]
  (if (= r (rem (+ step m) x))
    (+ step m)
    (recur (+ m step) r x step)))

(defn solution-2 [busses target step base]
  (if (empty? busses)
    base
    (let [first-b (first busses)
          first-t (first target)
          first-m (find-first base first-t first-b step)]
      (recur (rest busses) (rest target) (* step first-b) first-m))))

(defn solution-second-part []
  (let [lines '(17 37 449 23 13 19 607 41 29)
        target '(0 11 17 25 30 36 48 58 77)
        sanitized (map #(rem (- %1 (rem %2 %1)) %1) lines target)]
    (solution-2 lines sanitized 1 1)))
