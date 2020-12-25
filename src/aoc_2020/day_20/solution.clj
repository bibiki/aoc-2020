(ns aoc-2020.day-20.solution
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(def input (-> "resources/day-20-test.txt"
               slurp
               (s/split #"\n\n")))

(defn tile [t]
  (let [rows (s/split-lines t)
        id (s/split (first rows) #" ")
        tilele (map seq (drop 1 rows))]
    {:id (read-string (.substring (id 1) 0 4)) :tile tilele}))

(def tiles (->> input
               (mapv tile)))

(defn get-borders [one]
  (let [o (:tile one)
        north (first o)
        south (last o)
        east (map last o)
        west (map first o)]
    #{north south east west}))

(defn borders-in-common [one other]
  (if (= one other)
    #{}
    (let [o-borders (get-borders one)
          other-borders (get-borders other)
          other-borders-flipped (set (map reverse other-borders))]
      (sets/intersection o-borders (sets/union other-borders other-borders-flipped)))))

(defn line-up-two? [one others how-many]
  (let [b (map #(borders-in-common one %) others)
        bs (reduce sets/union b)]
    (= how-many (count bs))))

(defn solution-first-part []
  (let [t (filter #(line-up-two? % tiles 2) tiles)
        ids (map #(:id %) t)]
    (reduce * ids)))

(defn put-tile-on-image [image x y tile]
  (let [ilist (reduce concat image)
        index (+ y (* (count image) x))
        a (map-indexed #(if (= %1 index) tile %2) ilist)
        b (partition (count image) a)]
    (mapv vec b)))

(defn rotate-left
  ([tile] 
   (if (nil? tile)
     nil
     (rotate-left tile '())))
  ([tile rez]
   (if (empty? (reduce concat tile))
     rez
     (let [firsts (map first tile)
           rests (map rest tile)
           r (conj rez firsts)]
       (recur rests r)))))

(defn rotate-right [tile]
  (reverse (rotate-left tile)))

(defn flip-up [tile]
  (reverse tile))

(defn flip-side-to-side [tile]
  (-> rotate-left flip-up rotate-right))

(defn aligned? [one other]
  (if (or (empty? one) (empty? other))
    true
    (let [on (map last one)
          ot (map first other)]
      (= on ot))))

(defn is-valid? [image x y]
  (let [up (if (= x 0) nil ((image (dec x)) y))
        left (if (= y 0) nil ((image x) (dec y)))
        c ((image x) y)
        dim (count image)
        bound (dec dim)]
    (and (aligned? (rotate-left up) (rotate-left c))
         (aligned? left c))))

(defn all-directions [tile]
  (let [l1 (rotate-left tile)
        l2 (rotate-left l1)
        l3 (rotate-left l2)
        f (flip-up tile)
        f1 (rotate-left f)
        f2 (rotate-left f1)
        f3 (rotate-left f2)]
    (set (list tile l1 l2 l3 f f1 f2 f3))))

(def corners (->> tiles
                  (filter #(line-up-two? % tiles 2))
                  (map #(:tile %))
                  (map #(all-directions %))
                  (reduce concat)))

(def edges (->> tiles
                (filter #(line-up-two? % tiles 3))
                (map #(:tile %))
                (map #(all-directions %))
                (reduce concat)))

(def inner (->> tiles
                (filter #(line-up-two? % tiles 4))
                (map #(:tile %))
                (map #(all-directions %))
                (reduce concat)))

(defn what-tiles? [corners edges inner x y row-count]
  (let [d (dec row-count)]
    (cond
      (or (= x y 0) (= x y d) (and (= x 0) (= y d)) (and (= x d) (= y 0))) corners
      (or (= x d) (= x 0)) edges
      (or (= y d) (= y 0)) edges
      :else inner)))

(defn build-images [image index corners edges inner row-count]
  (if (= index (* (count image) (count image)))
    image
    (let [x (quot index row-count)
          y (rem index row-count)
          w-tiles (what-tiles? corners edges inner x y row-count)
          image-tiles (reduce concat image)
          all-image-tiles (mapcat #(all-directions %) image-tiles)
          n-tiles (filter #(not (contains? (set all-image-tiles) %)) w-tiles)
          images (map #(put-tile-on-image image x y %) n-tiles)
          valid (filter #(is-valid? % x y) images)
          ]
      valid)))

(defn build- [images index corners edges inner max-index]
  (println index)
  (if (= max-index index)
    images
    (let [row-count (int (Math/sqrt max-index))
          next-gen (map #(build-images % index corners edges inner row-count) images)
          next-images (reduce concat next-gen)]
      (recur next-images (inc index) corners edges inner max-index))))


(defn remove-borders [tile]
  (let [t (drop-last (drop 1 tile))]
    (map #(drop-last (drop 1 %)) t)))

(def monster '((\x \x \x \x \x \x \x \x \x \x \x \x \x \x \x \x \x \x \# \x)
 (\# \x \x \x \x \# \# \x \x \x \x \# \# \x \x \x \x \# \# \#)
 (\x \# \x \x \# \x \x \# \x \x \# \x \x \# \x \x \# \x \x \x)))

(defn scan-for-monster [monster suspect]
  (let [[ma mb mc] monster
        [sa sb sc] suspect
        sca (map #(if (= %1 \x) \x %2) ma sa)
        scb (map #(if (= %1 \x) \x %2) mb sb)
        scc (map #(if (= %1 \x) \x %2) mc sc)]
    (list sca scb scc)))

(defn count-monsters [monster sea row col rez]
  (cond
       (> row (- (count sea) 2)) rez
       (> col (- (count sea) 19)) (recur monster sea (inc row) 0 rez)
       :else (let [area (take 3 (drop row sea))
                   m (map #(take 20 (drop col %)) area)
                   scan (scan-for-monster monster m)
                   r (if (= scan monster) (inc rez) rez)]
               (recur monster sea row (inc col) r))))

(defn memory [trito corners edges inner]
  (build- '([[nil nil nil] [nil nil nil] [nil nil nil]]) 0 trito corners edges inner))

(defn join-two-tiles [t1 t2]
  (if (nil? t1)
    t2
    (map concat t1 t2)))

(defn join-tiles [tiles]
  (reduce join-two-tiles nil tiles))

(defn- count-monsters-in-sea [sea]
  (let [dim (int (Math/sqrt (count tiles)))
        rem-borders (map #(remove-borders %) (reduce concat sea))
        i (map #(join-tiles %) (partition dim rem-borders))
        ii (partition (* dim 8) (flatten i))
        all-seas (all-directions ii)
        counts (map #(count-monsters monster % 0 0 0) all-seas)
        max-count (reduce max counts)
        monster-c (count (filter #(= \# %) (flatten monster)))
        total-c (count (filter #(= \# %) (flatten ii)))]
    (- total-c (* monster-c max-count))))

(defn find-seas []
  (let [dim (int (Math/sqrt (count tiles)))
        trito (reduce concat (map #(all-directions %) (map #(:tile %) tiles)))
        frame (mapv vec (partition dim (repeat (* dim dim) nil)))
        seas (build- (list frame) 0 corners edges inner (* dim dim))]
    (map #(solution-second-part %) seas)))
