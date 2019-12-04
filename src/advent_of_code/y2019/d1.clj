(ns advent_of_code.y2019.d1
  (:require [clojure.java.io :as io]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d1.input"))))

(def num-input
  (map #(Integer/parseInt %) input))

; p1
(defn fuel [mass]
  (dec (dec (int (/ mass 3)))))

(->> num-input
     (map fuel)
     (reduce +))

; test for p1
(= (fuel 12) 2)
(= (fuel 14) 2)
(= (fuel 1969) 654)
(= (fuel 100756) 33583)

; p2
(def m-fuel (memoize fuel))

(defn fuel-amount [n]
  (let [n' (m-fuel n)]
    (if (< n' 0)
      0
      (+ n' (fuel-amount n')))))

(->> num-input
     (map fuel-amount)
     (reduce +))

; test for p2
(= (fuel-amount 14) 2)
(= (fuel-amount 1969) 966)
(= (fuel-amount 100756) 50346)
