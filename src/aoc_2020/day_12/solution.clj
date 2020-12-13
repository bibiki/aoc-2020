(ns aoc-2020.day-12.solution
  (:require [clojure.string :as s]))

(def in (s/split-lines (slurp "resources/day-12.txt")))

(def inn (map #(vector (.substring % 0 1) (Integer/parseInt (.substring % 1))) in))

(defn forward [steps dir north east]
  (cond 
    (= dir :east) [north (+ east steps)]
    (= dir :west) [north (- east steps)]
    (= dir :north) [(+ north steps) east]
    (= dir :south) [(- north steps) east]))

(defn next-dir [dir i in]
  (let [dirs {:north 0 :east 1 :south 2 :west 3}
        dir-steps (/ in 90)
        c (dir dirs)
        clock-wise (if (= "L" i) - +)]
    ([:north :east :south :west] (rem (+ 4 (clock-wise c dir-steps)) 4))))

(defn solution [input dir north east]
  (if (empty? input)
    (+ (Math/abs north) (Math/abs east))
    (let [instruction (first input)
          i (first instruction)
          in (last instruction)
          new-input (rest input)]
      (cond
        (= "N" i) (recur new-input dir (+ north in) east)
        (= "S" i) (recur new-input dir (- north in) east)
        (= "E" i) (recur new-input dir north (+ east in))
        (= "W" i) (recur new-input dir north (- east in))
        (= "F" i) (let [n-e (forward in dir north east)]
                    (recur new-input dir (n-e 0) (n-e 1)))
        (contains? #{"R" "L"} i) (recur new-input (next-dir dir i in) north east)
        ))))

(defn solution-first-part []
  (solution inn :east 0 0))


(defn next-way-point [w-north w-east dir degrees]
  (cond
    (= degrees 180) [(- w-north) (- w-east)]
    (and (= "L" dir) (= degrees 90)) [w-east (- w-north)]
    (and (= "R" dir) (= degrees 90)) [(- w-east) w-north]
    (and (= "L" dir) (= degrees 270)) [(- w-east) w-north]
    (and (= "R" dir) (= degrees 270)) [w-east (- w-north)]))

(defn solution-2 [input dir north east w-north w-east]
  (if (empty? input)
    (+ (Math/abs north) (Math/abs east))
    (let [instruction (first input)
          i (first instruction)
          in (last instruction)
          new-input (rest input)]
      (cond
        (= "N" i) (recur new-input dir north east (+ in w-north) w-east)
        (= "S" i) (recur new-input dir north east (- w-north in) w-east)
        (= "E" i) (recur new-input dir north east w-north (+ w-east in))
        (= "W" i) (recur new-input dir north east w-north (- w-east in))
        (= "F" i) (recur new-input dir (+ north (* in w-north)) (+ east (* in w-east)) w-north w-east)
        (contains? #{"R" "L"} i) (let [wne (next-way-point w-north w-east i in)]
                    (recur new-input dir north east (wne 0) (wne 1)))
        ))))

(defn solution-second-part []
  (solution-2 inn :east 0 0 1 10))
