(ns aoc-2020.day-23.solution)

(defn destination [current three m]
  (let [c (if (= 1 current) m (dec current))]
    (if (contains? (set three) c)
      (recur c three m)
      c)))

(defn first-f [ll k rez]
  (if (= 8 (count rez))
    rez
    (let [n (get ll k)]
      (recur ll n (str rez n)))))

(defn second-f [ll]
  (let [a (get ll 1)
        b (get ll a)]
    (* a b)))

(defn get-next-three [ll current]
  (let [a (get ll current)
        b (get ll a)
        c (get ll b)]
    [a b c]))

(defn solution [ll current cnt r m]
  (if (= cnt r)
    ll
    (let [[a b c] (get-next-three ll current)
          dest (destination current [a b c] m)
          after-dest (get ll dest)
          ll-a (assoc ll dest a)
          ll-b (assoc ll-a c after-dest)
          after-c (get ll c)
;          ddd (println dest a b c after-dest after-c)
          ll-c (assoc ll-b current after-c)]
      (recur ll-c after-c (inc cnt) r m))))

(defn solution-first-part []
  (let [input '(9 7 4 6 1 8 3 5 2)
        i (partition 2 1 input)
        ii (cons '(2 9) i)
        mmap (into {} (map vec ii))
        ll (solution mmap 9 0 100 9)]
    (first-f ll 1 "")))

(defn solution-second-part []
  (let [input '(9 7 4 6 1 8 3 5 2)
        resto (range 10 1000001)
        i (concat input resto)
        ii (partition 2 1 i)
        iii (cons '(1000000 9) ii)
        mmap (into {} (map vec iii))
        ll (solution mmap 9 0 10000000 1000000)]
    (second-f ll)))
