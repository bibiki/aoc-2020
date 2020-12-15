(ns aoc-2020.day-15.solution)

(def input [0 1 5 10 3 12 19])

(defn solution [input l c limit]
  (if (= c limit)
    l
    (let [ls (filter #(= l %) input)]
      (if (= 0 (count ls))
        (recur (conj input l) 0 (inc c) limit)
        (let [indexed (map-indexed #(vector %1 %2) input)
              filtered (filter #(= (% 1) l) indexed)
              last-two (last filtered)
              n (- c (last-two 0))]
          (recur (conj input l) n (inc c) limit))))))


(defn solution-2 [input l c limit]
  (if (= c limit)
    l
    (if (nil? (get input l))
      (recur (assoc input l (list c)) 0 (inc c) limit)
      (let [indices (get input l)
            m (first indices)
            n (- c m)]
        (recur (assoc input l (list c m)) n (inc c) limit)))))
