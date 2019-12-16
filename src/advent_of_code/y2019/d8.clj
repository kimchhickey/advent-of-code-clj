(ns advent_of_code.y2019.d8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d8.input"))))

(def n-input
  (map #(Character/digit % 10) (first input)))

(def width 25)
(def height 6)

(def layers (partition (* width height) n-input))

(get (frequencies (first layers)) 9 0)

(def fewer-zero
  (fn [layer]
    (get layer 0 0)))

; p1
(first (sort-by fewer-zero (->> layers
                                (map frequencies))))


; p2
(def layers-2d
  (->> layers
       (map #(partition width %))))

(defn vec2d
  "Return an x by y vector with all entries equal to val."
  [x y val]
  (vec (repeat y (vec (repeat x val)))))

(def canvas
  (vec2d 26 6 2))

(defn paint-pixel [p1 p2]
  (if (= 2 p1)
    p2
    p1))

(defn paint-row [v1 v2]
  (map paint-pixel v1 v2))

(def paint
  (fn [canvas layer]
    (map paint-row canvas layer)))

(def sol (->> layers-2d
              (reduce paint canvas)))
