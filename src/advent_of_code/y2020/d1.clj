(ns advent_of_code.y2020.d1
  (:require [advent_of_code.util :as util]))

(def input
  (->> (util/read-input "y2020/d1.input")
       (map #(Integer/parseInt %))))

(defn is2020? [v]
  (= 2020 (:sum v)))

(def p1
  (for [x input
        y input]
    {:x x
     :y y
     :sum (+ x y)}))

(let [{:keys [x y]} (util/find-first is2020? p1)]
  (* x y))

(def p2
  (for [x input
        y input
        z input]
    {:x x
     :y y
     :z z
     :sum (+ x y z)}))

(let [{:keys [x y z]} (util/find-first is2020? p2)]
  (* x y z))

;; :when
(set (for [x input
           y input
           :when (= 2020 (+ x y))]
       (* x y)))

(set (for [x input
           y input
           z input
           :when (= 2020 (+ x y z))]
       (* x y z)))
