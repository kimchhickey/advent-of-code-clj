(ns advent_of_code.y2020.d2
  (:require [advent_of_code.util :as util]
            [clojure.string :as str]))

(defn parse-line [s]
  (let [[_ min max c s] (re-find #"(\d+)-(\d+) (.): (.+)" s)]
    {:min (Long/parseLong min)
     :max (Long/parseLong max)
     :char (first c)
     :str s
     :freq (frequencies s)}))

(def input
  (->> (util/read-input "y2020/d2.input")
       (map parse-line)))

(defn valid? [m]
  (let [{:keys [min max char freq]} m
        target (get freq char 0)]
    (<= min target max)))

; p1
(->> input
     (map #(valid? %))
     (filter #(= true %))
     (count))

(defn p2-valid? [m]
  (let [{:keys [min max char str]} m
        target (set [(nth str (dec min)) (nth str (dec max))])]
    (and (contains? target char)
         (= 2 (count target)))))

;; p2
(->> input
     (map #(p2-valid? %))
     (filter #(= true %))
     (count))
