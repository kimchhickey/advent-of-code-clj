(ns advent_of_code.y2017.d2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "y2017/d2.input"))))

(defn parse
  [line]
  (str/split line #"\s"))

(defn parse-int
  [line]
  (map #(Integer. %) line))

(defn min-line
  [line]
  (apply min line))

(defn max-line
  [line]
  (apply max line))

(defn diff
  [line]
  (- (last line) (first line)))

;; p1
(->> input
     (map parse)
     (map parse-int)
     (map (juxt min-line max-line))
     (map diff)
     (flatten)
     (reduce + 0))

(defn divisible?
  [val coll]
  (reduce
    (fn [result candidate]
      (if (= candidate val)
        result
        (if (not= 0 (rem candidate val))
          result
          (conj result (/ candidate val)))))
    []
    coll))

(defn divisible-line
  [line]
  (map #(divisible? % line) line))

;; p2
(->> input
     (map parse)
     (map parse-int)
     (map divisible-line)
     (flatten)
     (apply +))
