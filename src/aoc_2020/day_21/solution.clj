(ns aoc-2020.day-21.solution
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(def input (-> "resources/day-21.txt" slurp s/split-lines))

(defn expand [v]
  (let [allergens (v 0)
        ingredients (v 1)]
    (map #(vector % ingredients) allergens)))

(def products (->> input
                  (map #(s/split % #" \(contains "))
                  (map #(vector (% 1) (s/split (% 0) #" ")))
                  (map #(vector (s/replace (% 0) #"\)" "") (% 1)))
                  (map #(vector (s/split (% 0) #", ") (% 1)))
                  (map expand)
                  (reduce concat)))

(def ingredients (->> products (map #(% 1)) distinct))

(def allergens (->> products (map #(% 0)) distinct))

(defn by-allergen [allergen products]
  (vector allergen
          (->> products
               (filter #( = allergen (% 0)))
               (map #(set (% 1))))))

(def grouped-by-allergen (map #(by-allergen % products) allergens))

(defn map-allergens-to-names [allergen-product]
  (let [a (first allergen-product)
        b (last allergen-product)
        p (reduce sets/intersection b)]
    (vector a p)))

(def all-allergens-names (reduce sets/union 
                                 (map #(% 1) (map map-allergens-to-names grouped-by-allergen))))

(defn solution-first-part []
  (reduce + (map #(if (contains? all-allergens-names %) 0 1) (reduce concat ingredients))))
