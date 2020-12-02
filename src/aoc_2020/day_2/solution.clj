(ns aoc-2020.day-2.solution
  (:require [clojure.string :as s]))

(defn is-valid? [l u letter password]
  (let [my-ls (filter #(= % letter) password)
        t (count my-ls)]
    (and (<= t u) (>= t l))))

(defn is-valid-second-version? [l u letter password]
  (let [my-letters-vec (vec (seq password))
        l-index (dec l)
        u-index (dec u)
        f (my-letters-vec l-index)
        l (my-letters-vec u-index)]
    (or (and (= f letter) (not= l letter))
        (and (= l letter) (not= f letter)))))

(defn extract [l]
  (let [tokens (s/split l #" ")
        nums (s/split (first tokens) #"-")
        letter (first (seq (first (drop 1 tokens))))
        password (last tokens)
        l (read-string (first nums))
        u (read-string (last nums))]
    (list l u letter password)))

(defn- solution [validator]
  (let [passwords (s/split (slurp "resources/day-2.txt") #"\n")
        extracted (map extract passwords)
        valid-passwords (filter #(apply validator %) extracted)]
    (count valid-passwords)))

(defn solution-first-part []
  (solution is-valid?))

(defn solution-second-part []
  (solution is-valid-second-version?))
