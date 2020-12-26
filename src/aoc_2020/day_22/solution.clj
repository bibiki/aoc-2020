(ns aoc-2020.day-22.solution)

(def player-a [25 37 35 16 9 26 17 5 47 32 11 43 40 15 7 19 36 20 50 3 21 34 44 18 22])

(def player-b [12 1 27 41 4 39 13 29 38 2 33 28 10 6 24 31 42 8 23 45 46 48 49 30 14])


(defn play [player-a player-b]
  (if (or (empty? player-b) (empty? player-a))
      (let [ps (if (empty? player-a) player-b player-a)
            scores (reverse (map inc (range (count player-a))))]
        (reduce + (map * ps scores)))
      (let [a-card (first player-a)
            b-card (first player-b)]
        (if (> a-card b-card)
          (recur (-> player-a (conj a-card) (conj b-card) rest vec)
                 (-> player-b rest vec))
          (recur (-> player-a rest vec)
                 (-> player-b (conj b-card) (conj a-card) rest vec))))))

(defn- round [p a b]
  (-> p (conj a) (conj b) rest vec))

(defn count-occurences [element l]
  (reduce + (map #(if (= element %) 1 0) l)))

(defn won-by-rounds [player]
  (let [h (:history player)
        deck (:deck player)]
    (contains? h deck)))

(defn start-player [deck]
  {:won false :deck deck :history #{}})

(defn play-recursive [game]
  (cond
    (:finished game) game
    (or (won-by-rounds (:a game)) (won-by-rounds (:b game)))
    (assoc game :finished true :a (assoc (:a game) :won true))
    :else
    (let [deck-a (:deck (:a game))
          deck-b (:deck (:b game))
          card-a (first deck-a)
          card-b (first deck-b)
          history-a (conj (:history (:a game)) deck-a)
          history-b (conj (:history (:b game)) deck-b)
          deck-a-lost (-> deck-a rest vec)
          deck-a-won (-> deck-a-lost (conj card-a) (conj card-b))
          deck-b-lost (-> deck-b rest vec)
          deck-b-won (-> deck-b-lost (conj card-b) (conj card-a))
          next-a-w {:deck deck-a-won :won false :history history-a}
          next-a-l {:deck deck-a-lost :won false :history history-a}
          next-b-w {:deck deck-b-won :won false :history history-b}
          next-b-l {:deck deck-b-lost :won false :history history-b}
          cc-a (dec (count deck-a))
          cc-b (dec (count deck-b))]
      (cond
        (empty? deck-b) (assoc game :a (assoc (:a game) :won true))
        (empty? deck-a) (assoc game :b (assoc (:b game) :won true))
        
        (and (>= cc-a card-a) (>= cc-b card-b)) (let [giga {:a (start-player (take card-a (rest deck-a))) :b (start-player (take card-b (rest deck-b)))}
                                                       rr (play-recursive giga)
                                                       a (:a rr)
                                                       b (:b rr)]
                                                  (if (:won a)
                                                    (recur {:a next-a-w :b next-b-l})
                                                    (recur {:a next-a-l :b next-b-w})))
        (> card-a card-b) (recur {:a next-a-w :b next-b-l})
        (> card-b card-a) (recur {:a next-a-l :b next-b-w})))))
