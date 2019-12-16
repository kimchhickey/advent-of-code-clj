(ns advent_of_code.y2019.d8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d8.test.input"))))

(def n-input
  (map #(Character/digit % 10) (first input)))

(def width 25)
(def height 6)

(def layers (partition (* width height) n-input))

(get (frequencies (first layers)) 9 0)

(def fewer-zero
  (fn [layer]
    (get layer 0 0)))

(first (sort-by fewer-zero (->> layers
                                (map frequencies))))

(defn paint [point output]
  (let [x (get point 0)
        y (get point 1)
        canvas (:canvas output)
        layer (:layer output)
        val-of-canvas (get (get canvas y) x)
        val-of-layer (get (get layer y) x)]
    (if (= 2 val-of-canvas)
      {:layer layer
       :canvas (assoc-in canvas [y x] val-of-layer)}
      output)))

(paint [0 0] {:layer [[1 1 1] [1 1 1]] :canvas [[0 2 2] [2 2 2]]})
(paint [0 1] {:layer [[1 1 1] [1 1 1]] :canvas [[2 2 2] [2 2 2]]})
(paint [1 0] {:layer [[1 1 1] [1 1 1]] :canvas [[2 2 2] [2 2 2]]})
(paint [1 1] {:layer [[1 1 1] [1 1 1]] :canvas [[2 2 2] [2 2 2]]})
(paint [2 0] {:layer [[1 1 1] [1 1 1]] :canvas [[2 2 2] [2 2 2]]})
(paint [2 1] {:layer [[1 1 1] [1 1 1]] :canvas [[2 2 2] [2 2 2]]})

(def draw
  (fn [layer canvas]
    (let [points (for [x (range 25)
                       y (range 6)]
                   [x y])]
      (reduce paint {:layer layer :canvas canvas} points))))

(draw layer canvas)

(defn vec2d
  [x y val]
  (vec (repeat y (vec (repeat x val)))))

(def canvas
  (vec2d 25 6 2))

(def layer
  (first (map #(partition 25 %) layers)))

(def o (->> layers
            (map #(partition 25 %))
            (reduce draw canvas)))
