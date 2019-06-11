; --- Day 2: Inventory Management System ---
; https://adventofcode.com/2018/day/2
(ns advent_of_code.y2018.d2
  (:require [clojure.java.io :as io]))

(def input
  (line-seq (io/reader (io/resource "y2018/d2.input"))))

(defn n-times? [n]
  (fn [coll]
    (not (empty? (filter #(= n (second %)) coll)))))

(def two-times? (n-times? 2))
(def three-times? (n-times? 3))

(defn inc-true [acc val]
  (if (true? val) (inc acc) acc))

(defn inc-true-vec [acc val]
  [(inc-true (first acc) (first val))
   (inc-true (second acc) (second val))])

; p1
(->> input
     (map #(frequencies (seq %)))
     (map (juxt two-times? three-times?))
     (reduce inc-true-vec [0 0])
     (reduce *))
