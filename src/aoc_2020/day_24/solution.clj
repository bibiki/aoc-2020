(ns aoc-2020.day-24.solution
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(def input (-> "resources/day-24.txt" slurp s/split-lines))

(def input-test (-> "resources/day-24-test.txt" slurp s/split-lines))

(def dirs {"ne" [1 1] "nw" [-1 1] "se" [1 -1] "sw" [-1 -1] "e" [2 0] "w" [-2 0]})

(defn what-dir? [two-letters]
  (let [d (reduce str two-letters)]
    (cond
      (contains? #{"ne" "nw" "se" "sw"} d) d
      :else (str (first two-letters)))))

(defn update-history [history pos]
  (let [l (get history pos)]
    (if (or (= nil l) (= :white l))
      (assoc history pos :black)
      (assoc history pos :white))))

(defn tile [dirs line pos]
  (if (empty? line)
    pos
    (let [d (take 2 line)
          dir (what-dir? d)
          dir-val (get dirs dir)
          pos-next (map + pos dir-val)
          line-next (.substring line (count dir))]
      (recur dirs line-next pos-next))))

(defn get-layout [input]
  (let [hs (map #(tile dirs % [0 0]) input)]
    (reduce update-history {} hs)))

(defn solution-first-part []
  (let [layout (get-layout input)]
    (->> layout (map #(% 1)) (filter #(= :black %)) count)))

(defn sum- [a b]
  (map + a b))

(defn count-black-neighbors [tile layout dirs]
  (let [neighbors (map #(sum- tile %) (vals dirs))
        neighbor-colors (map #(or (get layout %) :white) neighbors)]
    (count (filter #(= :black %) neighbor-colors))))

(defn next-color [a layout]
  (let [t (first a)
        cnt (last a)
        color (or (get layout t) :white)]
    (cond
      (and (not (contains? #{1 2} cnt)) (= :black color)) [t :white]
      (and (= 2 cnt) (= :white color)) [t :black]
      :else [t color])))

(defn get-neighbors [a dirs]
  (map #(sum- a %) dirs))

(defn expand [init steps]
  (if (= 0 steps)
    init
    (let [n (mapcat #(get-neighbors % (vals dirs)) init)]
      (recur (distinct n) (dec steps)))))

(defn solution-second [layout dirs cnt m]
  (if (= cnt m)
    (->> layout (map #(% 1)) (filter #(= :black %)) count)
    (let [tiles (keys layout)
          neighbors (mapcat #(get-neighbors % (vals dirs)) tiles)
          n (sets/difference (set neighbors) (set tiles))
          color-neighbors (map #(vector % :white) n)
          layout-expanded (into layout color-neighbors)
          a (map #(vector % (count-black-neighbors % layout-expanded dirs)) (keys layout-expanded))
          b (map #(next-color % layout-expanded) a)]
      (recur (into {} b) dirs (inc cnt) m))))

(defn solution-second-part []
  (let [layout (get-layout input)]
    (solution-second layout dirs 0 100)))
