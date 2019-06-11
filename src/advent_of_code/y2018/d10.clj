; --- Day 10: The Stars Align ---
; https://adventofcode.com/2018/day/10
(ns advent_of_code.y2018.d10)

(def frame (java.awt.Frame.))

(.setVisible frame true)

(.setSize frame (java.awt.Dimension. 500 500))

(def gfx (.getGraphics frame))

(do
  (.setColor gfx (java.awt.Color. 0 0 0))
  (.fillRect gfx 50 50 10 10))
