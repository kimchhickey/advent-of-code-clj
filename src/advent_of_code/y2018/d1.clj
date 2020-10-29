(ns advent_of_code.y2018.d1
  (:require [clojure.java.io :as io]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2018/d1.input"))))

(def num-input (map read-string input))

; p1
(time (apply + num-input))

; p2 - solution 1
(reduce
 (fn [result freq]
   (let [curr (+ (or (last result) 0) freq)]
     (if (result curr)
       (reduced curr)
       (conj result curr))))
 {}
 (cycle num-input))

(defn find-first-duplicate [xs]
  (let [result (reduce (fn [seen x]
                         (if (seen x)
                           (reduced x)
                           (conj seen x)))
                       #{}
                       xs)]
    (if (set? result)
      nil
      result)))

; p2 - solution 2
(time (find-first-duplicate (reductions + (cycle num-input))))
