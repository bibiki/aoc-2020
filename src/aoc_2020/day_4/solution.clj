(ns aoc-2020.day-4.solution
  (:require [clojure.string :as s]))

(def input (slurp "resources/day-4.txt"))

(def passport-strings (s/split input #"\n\n"))

(defn- to-kv-pair [in]
  (let [kv (s/split in #":")]
    [(keyword (kv 0)) (kv 1)]))

(defn is-valid? [p]
  (and (:eyr p) (:byr p) (:hgt p) (:iyr p) (:hcl p) (:ecl p) (:pid p)))

(defn string-to-passport [in]
  (let [tokens (s/split in #"\n| ")
        kv-pairs (map to-kv-pair tokens)]
    (into {} kv-pairs)))

(defn solution-first-part []
  (let [passports (map string-to-passport passport-strings)]
    (count (filter is-valid? passports))))

(defn- is-between? [x l u] (let [t (read-string x)] (< l t u)))

(defn- is-valid-byr? [byr] (is-between? byr 1919 2003))
(defn- is-valid-iyr? [iyr] (is-between? iyr 2009 2021))
(defn- is-valid-eyr? [eyr] (is-between? eyr 2019 2031))

(defn- is-valid-hgt? [hgt]
  (let [unit (drop-while #(and (not= \c %) (not= \i %)) (seq hgt))
        nums (take-while #(and (not= \c %) (not= \i %)) (seq hgt))
        u (reduce str unit)
        n (reduce str nums)]
        (if (and (not= "cm" u) (not= "in" u))
          false
          (if (= "cm" u)
            (is-between? n 149 194)
            (is-between? n 58 77)))))

(defn all-nums? [l]
  (let [ns #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
        nums (map #(contains? ns %) l)]
    (reduce #(and %1 %2) true nums)))

(defn all-letters? [l]
  (let [chars #{\a \b \c \d \e \f \0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
        ls (map #(contains? chars %) l)]
    (reduce #(and %1 %2) true ls)))

(defn- is-valid-hcl? [hcl]
  (let [h (seq hcl)
        resto (drop 1 h)]
        (and (= \# (first h)) (= 6 (count resto)) (all-letters? resto))))

(defn- is-valid-ecl? [ecl]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(defn- is-valid-pid? [pid]
  (let [ds (seq pid)]
    (and (= 9 (count ds)) (all-nums? ds))))

(defn is-valid-passport? [p]
  (let [res (and
             (and (:byr p) (is-valid-byr? (:byr p)))
             (and (:iyr p) (is-valid-iyr? (:iyr p)))
             (and (:eyr p) (is-valid-eyr? (:eyr p)))
             (and (:hgt p) (is-valid-hgt? (:hgt p)))
             (and (:hcl p) (is-valid-hcl? (:hcl p)))
             (and (:ecl p) (is-valid-ecl? (:ecl p)))
             (and (:pid p) (is-valid-pid? (:pid p))))]
     res))

(defn solution-second-part []
  (let [passports (map string-to-passport passport-strings)
        valid-passports (filter is-valid-passport? passports)]
    (println valid-passports)
    (count valid-passports)))
