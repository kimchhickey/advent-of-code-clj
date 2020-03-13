(ns advent_of_code.y2019.d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d6.input"))))

(defn parse [s]
  (str/split s #"\)"))

(def orbit (first (map parse (take 10 input))))

(defn ->key [x]
  {:left  (keyword (str/lower-case (get x 0)))
   :right (keyword (str/lower-case (get x 1)))})

(def ->orbit-map
  (fn [orbit orbit-map]
    (if (contains? orbit-map (:left orbit))
      (assoc-in orbit-map (:left orbit) (conj (:left orbit-map) (:right orbit)))
      (assoc orbit-map (:left orbit) [(:right orbit)]))))

(def orbit-map (->> input
                    (map parse)
                    (map ->key)
                    (reduce ->orbit-map {})))

(take 100 orbit-map)
