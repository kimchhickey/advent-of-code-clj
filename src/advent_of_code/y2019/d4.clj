(ns advent_of_code.y2019.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def begin 130254)
(def end 678275)

(def candidate (range begin end))

;; new version
(defn ->adjacent [number]
  (let [s (str number)]
    (partition 2 (rest (drop-last (interleave s s))))))

(defn adjacent [adj]
  (not-empty (filter #(= (first %) (second %)) adj)))

(defn decrease [adj]
  (empty? (filter #(> (int (first %)) (int (second %))) adj)))

(count (->> candidate
            (map ->adjacent)
            (filter adjacent)
            (filter decrease)))

;; old version
(defn same-adjacent [number]
  (> 6 (count (set (str number)))))

(defn never-decrease [number]
  (= (vec (str number)) (sort (str number))))

(defn no-partial-repeated [number]
  (let [f (frequencies (str number))]
    (not (nil? (some #{2} (map second f))))))

;; p1
(time (count (->> candidate
                  (filter same-adjacent)
                  (filter never-decrease)))) ;; "Elapsed time: 2730.494505 msecs"

;; p2
(time (count (->> candidate
                  (filter same-adjacent)
                  (filter never-decrease)
                  (filter no-partial-repeated)))) ;; => "Elapsed time: 2805.213083 msecs"

;; transducer version 
(def xform (comp
            (filter same-adjacent)
            (filter never-decrease)
            (filter no-partial-repeated)))

(defn rf1
  ([] 0)
  ([acc] acc)
  ([acc v]
   (inc acc)))

(time (transduce xform rf1 candidate)) ;; => "Elapsed time: 2851.674023 msecs"

(defn rf2
  ([] [])
  ([acc] (count acc))
  ([acc v]
   (conj acc v)))

(time (transduce xform rf2 candidate)) ;; => "Elapsed time: 2805.496663 msecs"

