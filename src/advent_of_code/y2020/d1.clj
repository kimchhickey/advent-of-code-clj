(ns advent_of_code.y2020.d1
  (:require [advent_of_code.util :as util]))

(def input
  (->> (util/read-input "y2020/d1.input")
       (map #(Integer/parseInt %))))

(count input)

(def p1
  (for [x (range 0 200)
        y (range 0 200)]
    (let [xval (nth input x)
          yval (nth input y)]
      {:x xval
       :y yval
       :sum (+ xval yval)})))

(def p2
  (for [x (range 0 200)
        y (range 0 200)
        z (range 0 200)]
    (let [xval (nth input x)
          yval (nth input y)
          zval (nth input z)]
      {:x xval
       :y yval
       :z zval
       :sum (+ xval yval zval)})))

(defn is2020 [x]
  (= 2020 (:sum x)))

(util/find-first is2020 p2)
