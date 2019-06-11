(ns advent_of_code.y2017.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "y2017/d4.input"))))

(defn parse
  [line]
  (str/split line #"\s"))

(defn diff-with-map
  [line]
  (= (count (into #{} line)) (count line)))

;; p1
(count
  (->> input
     (map parse)
     (filter diff-with-map)))

(defn sorted-char
  [line]
  (map #(sort (seq %)) line))

;; p2
(count
  (->> input
     (map parse)
     (map sorted-char)
     (filter diff-with-map)))
