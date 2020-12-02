(ns aoc-2020.core
  (:require [clojure.string :as s]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn get-list-of-numbers [filename]
  (let [f filename
        l (map read-string (s/split (slurp f) #"\n"))]
    l))
