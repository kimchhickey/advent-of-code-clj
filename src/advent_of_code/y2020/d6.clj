(ns advent-of-code.y2020.d6
  (:require [advent_of_code.util :as u]
            [clojure.set :as set]))

(def input
  (u/read-input "y2020/d6.input"))

;; p1
(defn parse-group [g]
  (into #{} (flatten (map #(seq (char-array %)) g))))

(->> input
     (partition-by #(= "" %))
     (filter #(not (= "" (first %))))
     (map parse-group)
     (map count)
     (apply +))

(defn parse-g [g]
  (->> g
       (map (fn [s]
              (map #(keyword (str %)) s)))
       (map #(into #{} %))
       ))

(keyword (str \a))

;; p2
(->> input
     (partition-by #(= "" %))
     (filter #(not (= "" (first %))))
     (map parse-g)
     (map #(apply set/intersection %))
     (map count)
     (apply +))
