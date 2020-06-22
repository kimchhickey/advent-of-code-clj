(ns advent_of_code.y2019.d10
  (:require [clojure.java.io :as io]
            [advent_of_code.util :as util]
            [clojure.algo.generic.math-functions :as generic-math]))

(def input
  (line-seq (io/reader (io/resource "y2019/d10.test.input"))))

(def stars
  (reduce 
   (fn [result row]
     (let [i (first row)
           js (->> (second row)
                   (map-indexed vector)
                   (filter #(= (second %) \#))
                   (map first))
           pairs (map vector js (iterate identity i))]
       (apply conj result pairs)
       ))
   []
   (map-indexed vector input)))

(defn slope
  [[x1 y1]]
  (fn [[x2 y2]]
    (generic-math/atan2 (- y1 y2) (- x1 x2))))

(defn count-detected-stars
  [me all]
  (->> all
       (filter #(not= me %))
       (map (fn [s]
              {:pos s
               :slope ((slope me) s)}))))

#_(count (into #{} (map :slope (count-detected-stars [11, 13] stars))))

#_(->> stars
     (map (fn [star] {:pos star
                      :count (count-detected-stars star stars)})))
