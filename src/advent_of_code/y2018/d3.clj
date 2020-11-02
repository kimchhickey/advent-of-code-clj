; --- Day 3: No Matter How You Slice It ---
; https://adventofcode.com/2018/day/3
(ns advent_of_code.y2018.d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "y2018/d3.input"))))

(defn parse-int [str f del]
  (Integer/parseInt (f (str/split str del))))

(defn parse-line [raw-line]
  (let [[id _ xy wh] (str/split raw-line #" ")]
    {:id id
     :x      (parse-int xy first #",|:")
     :y      (parse-int xy second #",|:")
     :width  (parse-int wh first #"x")
     :height (parse-int wh second #"x")}))

(defn generate-seq [m]
  (let [{:keys [x y width height]} m]
    (for [x' (range x (+ x width))
          y' (range y (+ y height))]
      [x' y'])))

(def claims
  (map parse-line input))

; p1
(time
 (->> claims
      (mapcat generate-seq)
      frequencies
      (filter #(> (second %) 1))
      count))

; p2n
(def one (first data))
(def two (second data))
(def three (second (rest data)))

(defn transform [m]
  (let [{:keys [id x y width height]} m]
    {:id id
     :left x
     :bottom y
     :right (+ x width)
     :top (+ y height)}))

(defn overlap-rect? [a b]
  (if (and (< (:left a) (:right b)) (> (:right a) (:left b))
           (> (:top a) (:bottom b)) (< (:bottom a) (:top b)))
    true
    false))

(defn is-intact-claim? [target claims]
  (reduce (fn [result claim]
            (if (and (overlap-rect? target claim) (not= (:id target) (:id claim)))
              (reduced nil)
              result))
          target
          claims))

(def claims-rect
  (map transform claims))

(->> claims-rect
     (map #(overlay-claims? % claims-rect))
     (filter #(not (nil? %))))


