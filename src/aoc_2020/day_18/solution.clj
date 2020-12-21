(ns aoc-2020.day-18.solution)

(def input (->> "resources/day-18.txt"
               slurp
               clojure.string/split-lines
               (map #(.replace % "(" "( "))
               (map #(.replace % ")" " )"))))

(defn can-operate? [a b c]
  (and (contains? #{"+" "*"} b)
       (every? #(Character/isDigit %) a)
       (every? #(Character/isDigit %) c)))

(defn is-group? [a b c]
  (and (= a "(") (= c ")") (every? #(Character/isDigit %) b)))

(defn get-func [f]
  (if (= "+" f) + *))

(defn solution [input i]
  (if (= 1 (count input))
    input
    (let [[a b c] (take 3 (drop i input))]
      (cond (can-operate? a b c)
            (let [r ((get-func b) (read-string a) (read-string c))]
              (recur (concat (take i input) (list (str r)) (drop (+ i 3) input)) 0))
            (is-group? a b c)
              (recur (concat (take i input) (list b) (drop (+ 3 i) input)) 0)
            :else (recur input (inc i))))))

(defn solution-first-part []
  (reduce + (map #(read-string (first %)) (map #(solution % 0) (map #(clojure.string/split % #" ") input)))))

(defn try-sum [a b c d e]
  (if (and (= "+" c) (can-operate? b c d))
    (+ (read-string b) (read-string d))))

(defn try-group [a b c d e]
  (if (and (can-operate? b c d) (= "(" a) (= ")" e))
    ((get-func c) (read-string b) (read-string d))))

(defn try-multiply [a b c d]
  (if (= "*" b d)
    (* (read-string a) (read-string c))))

(defn ungroup [a b c]
  (if (and (= ")" c) (= "(" a))
    b))

(defn new-input [input i skip r]
  (concat (take i input)
          (list (str r))
          (drop (+ i skip) input)))

(defn solution-second [input i]
  (if (= 3 (count input))
    (let [[a b c] input]
      ((get-func b) (read-string a) (read-string c)))
    (let [[a b c d e] (take 5 (drop i input))
          ff (try-sum "+" a b c "+")
          uu (ungroup a b c)
          gg (try-multiply a b c d)
          t (try-sum a b c d e)
          q (try-group a b c d e)]
      (cond
        (or uu ff gg) (recur (new-input input i 3 (or uu ff gg)) 0)
        t (recur (concat (take i input) (list a (str t) e) (drop (+ i 5) input)) 0)
        q (recur (new-input input i 5 q) 0)

        :else (recur (filter #(not= nil %) input) (inc i))))))

(defn solution-second-part []
  (->> input
       (map #(clojure.string/split % #" "))
       (map #(solution-second % 0))
       (reduce +)))
