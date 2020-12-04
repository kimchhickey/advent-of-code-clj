(ns advent_of_code.y2020.d3
  (:require [advent_of_code.util :as util]))

(def input
  (util/read-input "y2020/d3.input"))

(defn slope
  ([]
   (slope 1 3))
  ([x y]
   (cons [x y] (lazy-seq (slope (* 2 x) (* 2 y))))))

(defn make-slope [x-step y-step]
  (fn slope
    ([]
     (slope x-step y-step))
    ([x y]
     (cons [x y] (lazy-seq (slope (+ x x-step) (+ y y-step)))))))

(def make-slope-3-1 (make-slope 3 1))

(take 10 (make-slope-3-1))

(rem 12 11)

(defn tree? [[x y] input]
  (if (>= y (count input))
    false
    (let [row (nth input y)
          x-max (count (first input))
          x' (rem x x-max)
          val (nth row x')]
      (if (= \# val)
        true
        false))))

(def p1-slope (take 322 (make-slope-3-1)))

(defn count-trees [slope]
  (->> slope
       (map #(tree? % input))
       (filter #(= true %))
       (count)))

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn ->seq [[x y]]
  (take 322 ((make-slope x y))))

(->> slopes
     (map ->seq)
     (map count-trees)
     (apply *))
