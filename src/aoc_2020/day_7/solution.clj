(ns aoc-2020.day-7.solution
  (:require [clojure.string :as s]))

(def in (s/split-lines (slurp "resources/day-7-test.txt")))

(def container-content (map #(s/split % #" bags contain ") in))

(defn my-contains? [stack needles]
  (< 0 (count (filter #(.contains stack %) needles))))

(defn solution [stack needles]
  (let [containers (filter #(my-contains? (% 1) needles) stack)
        additional-needles (map #(% 0) containers)
        new-needles (set (concat needles additional-needles))]
    (if (= new-needles needles)
      (dec (count needles))
      (recur stack new-needles))))

(defn multi [operand content]
  (if (= "no other bags." content)
    operand
    (let [bags (s/split content #", ")
          bag-counts (map #((s/split % #" ") 0) bags)
          bag-counts-upd (map #(* operand (Integer/parseInt %)) bag-counts)
          bags-without-counts (map #(drop 1 (s/split % #" ")) bags)
          res (map #(concat [%1] %2) bag-counts-upd bags-without-counts)]
      (vec (map #(s/join " " %) res)))))

(defn count-bags [operand bag stack]
  (let [bag-content (first (filter #(= bag (% 0)) stack))]
    (multi operand (bag-content 1))))

(defn mapper [bag-with-count stack]
  (let [to-vec (s/split bag-with-count #" ")
        c (read-string (first to-vec))
        name (str (to-vec 1) " " (to-vec 2))]
    (count-bags c name stack)))

(defn all-nums [l]
  (reduce #(and %1 (number? %2)) true l))

(defn solution-second-part [stack needles total]
  (println needles total)
  (if (all-nums? needles)
    total
    (let [found (filter number? needles)
          not-found (filter #(not (number? %)) needles)
          count-in-needles (map #(read-string ((s/split % #" ") 0)) not-found)
          next-needles (flatten (map #(mapper % stack) not-found))
          sum (reduce + count-in-needles)]
      (recur stack next-needles (+ total sum)))))
