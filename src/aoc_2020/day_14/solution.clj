(ns aoc-2020.day-14.solution
  (:require [clojure.string :as s]))

(def input (-> "resources/day-14.txt" slurp s/split-lines))

(defn mask-list [st]
  (seq (.substring st 7)))

(defn add-trailing-zeros [l]
  (concat (repeat (- 36 (count l)) 0) l))

(defn int->binary-list
  ([i] (int->binary-list i '()))
  ([i res]
   (if (= i 0)
     (add-trailing-zeros res)
     (recur (quot i 2) (cons (rem i 2) res)))))

(defn mem-input [st]
  (let [to-v (s/split st #" = ")
        m (Long/parseLong (.substring (to-v 0) 4 (dec (.length (to-v 0)))))
        v (Long/parseLong (to-v 1))]
    [m (int->binary-list v)]))

(defn mask-input [m in]
  (map #(if (= \X %1) %2 (read-string (str %1))) m in))

(defn list->int [l acc]
  (if (= 1 (count l))
    (+ (last l) (* 2 acc))
    (recur (rest l) (+ (first l) (* 2 acc)))))

(defn process [input memory mask]
  (if (empty? input)
    memory
    (let [instrc (first input)
          inp (rest input)]
      (cond
        (.contains instrc "mask") (recur inp memory (mask-list instrc))
        :else (let [v (mem-input instrc)
                    mem-loc (v 0)
                    v-in (mask-input mask (v 1))
                    val (list->int v-in 0)]
                (println v val)
                (recur inp (assoc memory mem-loc val) mask))))))

(defn mask-mem [mask mem res]
  (if (empty? mask)
    (map reverse res)
    (let [f-mask (first mask)
          f-mem (first mem)
          msk (rest mask)
          mm (rest mem)]
      (cond
        (= \0 f-mask) (recur msk mm (map #(cons f-mem %) res))
        (= \1 f-mask) (recur msk mm (map #(cons 1 %) res))
        :else (recur msk mm (map-indexed #(cons (if (< %1 (count res)) 1 0) %2) (concat res res)))))))

(defn process-2 [input memory mask]
  (if (empty? input)
    memory
    (let [instrc (first input)
          inp (rest input)]
      (cond
        (.contains instrc "mask") (recur inp memory (mask-list instrc))
        :else (let [v (mem-input instrc)
                    mem-loc (int->binary-list (v 0))
                    mem-masked (mask-mem mask mem-loc '(()))
                    mems (map #(list->int % 0) mem-masked)
                    v-in (list->int (v 1) 0)
                    mem (reduce #(assoc %1 %2 v-in) memory mems)]
                (recur inp mem mask))))))
