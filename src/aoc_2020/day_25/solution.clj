(ns aoc-2020.day-25.solution)

(def door 18356117)

(def card 5909654)


(defn transform [loop-size c subject rez]
  (if (= c loop-size)
    rez
    (let [r (* rez subject)
          rr (rem r 20201227)]
      (recur loop-size (inc c) subject rr))))

(defn crack-loop-size [target subject candidate c]
  (if (= target candidate)
    c
    (let [r (* candidate subject)
          rr (rem r 20201227)]
      (recur target subject rr (inc c)))))
