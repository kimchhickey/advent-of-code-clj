(ns advent_of_code.y2018.d1
  (:require [clojure.java.io :as io]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2018/d1.input"))))

; p1
(apply + (map read-string input))

; p2 - solution 1
(reduce
  (fn [result freq]
    (let [curr (+ (or (last result) 0) freq)]
      (if (some #(= curr %) result)
        (reduced curr)
        (conj result curr))))
  []
  (cycle (map read-string input)))

; p2 - solution 2
(util/first-duplicate (reductions + (cycle (map read-string input))))