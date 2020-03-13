(ns advent_of_code.y2019.d2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d2.input"))))

(def vec-input
  (let [opcodes (first input)]
    (map #(Integer/parseInt %) (str/split opcodes #","))))

(def real-input (assoc (vec vec-input) 1 12 2 2))

(defn next [addr]
  (+ 4 addr))

(defn op-1 [curr mem]
  (let [input1 (get mem (get mem (+ 1 curr)))
        input2 (get mem  (get mem (+ 2 curr)))
        output-pos (get mem (+ 3 curr))
        next-mem (assoc mem output-pos (+ input1 input2))]
    (run (+ 4 curr) next-mem)))

(defn op-2 [curr mem]
  (let [input1 (get mem (get mem (+ 1 curr)))
        input2 (get mem (get mem (+ 2 curr)))
        output-pos (get mem (+ 3 curr))
        next-mem (assoc mem output-pos (* input1 input2))]
    (run (+ 4 curr) next-mem)))

(defn run [curr mem]
  (case (get mem curr)
    1  (op-1 curr mem)
    2  (op-2 curr mem)
    99 (first mem)
    :something-wrong))
          
(run 0 real-input)

; p2
(def p2
  (for [x (range 0 99)
        y (range 0 99)]
    {:x x :y y :val (sm 0 (assoc (vec vec-input) 1 x 2 y))}))

(filter #(= 19690720 (:val %)) p2)
