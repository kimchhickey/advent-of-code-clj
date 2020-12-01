; --- Day 10: The Stars Align ---
; https://adventofcode.com/2018/day/10
(ns advent_of_code.y2018.d10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as u]))

(def input (u/read-input "y2018/d10.input"))
(def input-map
  (->> input
       (map (fn [s]
              (let [a (filter #(not= % "") (str/split s #"<|,|\s+|>"))
                    parse-num (fn [i] (Integer/parseInt (nth a i)))]
                {:pos [(parse-num 1)
                       (parse-num 2)]
                 :vel [(parse-num 4)
                       (parse-num 5)]})))))

(defn next-pos [[pos-x pos-y] [vel-x vel-y]]
  [(+ pos-x vel-x)
   (+ pos-y vel-y)])

(defn play [points]
  (map ))
