(ns advent_of_code.y2019.d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d3.input"))))

(defn parse [s]
  (let [dir (first s)
        dist (Integer/parseInt (str/join (rest s)))]
    {:dir dir :dist dist}))

(defn parse-wire [w]
  (map parse (str/split w #",")))

(def wire-one (parse-wire (first input)))
(def wire-two (parse-wire (second input)))

(vec wire-one)
(vec wire-two)

(defn point->line [x y direction dist]
  (case direction
    \R ; :right
    {:x (+ x dist) :y y
     :points (for [x' (range (inc x) (+ x (inc dist)))]
               [x' y])}
    \L
    {:x (- x dist) :y y
     :points (reverse (for [x' (range (- x dist) x)]
                        [x' y]))}
    \U
    {:x x :y (+ y dist)
     :points (for [y' (range (inc y) (+ y (inc dist)))]
               [x y'])}
    \D
    {:x x :y (- y dist)
     :points (reverse (for [y' (range (- y dist) y)]
                        [x y']))}
    :wrong-direction))

(defn w [vector]
  (reduce (fn [wire path]
            (let [x (:x wire)
                  y (:y wire)
                  points (:points wire)
                  dir (:dir path)
                  dist (:dist path)
                  result (point->line x y dir dist)]
              {:x (:x result)
               :y (:y result)
               :points (concat points (:points result))}))
          {:x 0 :y 0 :points []}
          vector))

(def w1 (w wire-one))
(def w2 (w wire-two))

(take 3 w1)
(:points w1)
(:points w2)

(def x (clojure.set/intersection (set (:points w1)) (set (:points w2))))

(first x)

(defn steps [key coll]
  (keep-indexed #(if (= key %2) %1) coll))

(defn add-sum [key]
  (let [w1 (vec (:points w1))
        w2 (vec (:points w2))]
    {:key key
     :sum (+ (first (steps key w2)) (first (steps key w1)))}))

(first x)
(steps (first x) (:points w1))
(steps (first x) (:points w2))

(first (sort (->> x
                  (map #(add-sum %))
                  (map #(:sum %)))))

(defn abs [n] (max n (- n)))

(sort (map (fn [[%1 %2]]
             (+ (abs %1) (abs %2))) x))


(def s1
  (conj #{} (point->line 0 990 \D 10)))

(first s1)

(def s2
  (conj #{} (point->line -10 0 \L 10)))

(first s2)

(clojure.set/union (first s1) (first s2))
