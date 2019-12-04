(ns advent_of_code.y2019.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def begin 130254)
(def end 678275)

(def candidate (range begin end))

(defn same-adjacent [number]
  (> 6 (count (set (str number)))))

(defn never-decrease [number]
  (= (vec (str number)) (sort (str number))))

(defn no-partial-repeated [number]
  (let [f (frequencies (str number))]
    (not (nil? (some #{2} (map second f))))))

(time (count (->> candidate
                  (filter same-adjacent)
                  (filter never-decrease)
                  (filter no-partial-repeated))))
