; --- Day 3: No Matter How You Slice It ---
; https://adventofcode.com/2018/day/3
(ns advent_of_code.y2018.d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "y2018/d3.input"))))

(defn parse-str [str f del]
  (Integer/parseInt (f (str/split str del))))

(defn parse-line [raw-line]
  (let [[id _ xy wh] (str/split raw-line #" ")]
    [id {:x      (parse-str xy first #",|:")
         :y      (parse-str xy second #",|:")
         :width  (parse-str wh first #"x")
         :height (parse-str wh second #"x")}]))

(defn gen-seq [m]
  (let [{:keys [x y width height]} m]
    (for [x' (range x (+ x width))
          y' (range y (+ y height))]
      [x' y'])))

(def data (->> (map parse-line input)
               (into {})))

; p1
(time
  (->> (vals data)
       (mapcat gen-seq)
       (frequencies)
       (filter #(> (second %) 1))
       (count)))

; p2
(def overlapped-points (->> (vals data)
                            (mapcat gen-seq)
                            (frequencies)
                            (filter #(> (second %) 1))
                            (map first)))

(defn some-point [rect point]
  (let [[x' y'] point
        {:keys [x y width height]} rect]
    (and (<= x x' (+ x width))
         (<= y y' (+ y height)))))

(defn some-points [[_ rect] points]
  (not-any? #(some-point rect %) points))

(->> data
     (filter #(some-points % overlapped-points))
     (map first)
     (first))