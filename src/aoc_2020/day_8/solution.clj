(ns aoc-2020.day-8.solution
  (:require [clojure.string :as s]))

(def in (s/split-lines (slurp "resources/day-8.txt")))

(def operation-operand (map #(s/split % #" ") in))

(defn sanitize [item]
  (vector (keyword (item 0)) (Integer/parseInt (item 1))))

(def op-op-sanitized (vec (map sanitize operation-operand)))

(defn halt-func-first [history counter op-count]
  (contains? history counter))

(defn halt-func-second [history counter op-count]
  (or (halt-func-first history counter op-count) (>= counter op-count)))

(defn switch-op [op-op i]
  (let [o (op-op i)
        changed (if (= :jmp (o 0)) :nop (if (= :nop (o 0)) :jmp (o 0)))
        new (vector changed (o 1))]
    (vec (concat (take i op-op) (cons new (drop (inc i) op-op))))))

(defn exec [op-op counter accumulator history halt-func]
  (if (halt-func history counter (count op-op))
    {:c counter :acc accumulator}
    (let [instruction (op-op counter)
          op (instruction 0)
          operand (instruction 1)
          history-upd (conj history counter)
          acc (if (= :acc op) (+ accumulator operand) accumulator)
          c (if (= :jmp op) (+ counter operand) (inc counter))]
      (recur op-op c acc history-upd halt-func))))

(defn solution-first-part []
  (exec op-op-sanitized 0 0 #{} halt-func-first))

(defn solution-second-part []
  (let [inputs (map #(switch-op op-op-sanitized %) (range 0 605))
        outputs (map #(exec % 0 0 #{} halt-func-second) inputs)]
    (filter #(<= 605 (:c %)) outputs)))
