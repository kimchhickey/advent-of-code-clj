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

(defn op-1 [current-opcode-index vector]
  (let [input1 (nth vector (get vector (+ 1 current-opcode-index)))
        input2 (nth vector  (get vector (+ 2 current-opcode-index)))
        output-position (get vector (+ 3 current-opcode-index))
        next-vector (assoc vector output-position (+ input1 input2))]
    (run (+ 4 current-opcode-index) next-vector)))

(defn op-2 [current-opcode-index vector]
  (let [input1 (nth vector (get vector (+ 1 current-opcode-index)))
        input2 (nth vector (get vector (+ 2 current-opcode-index)))
        output-position (get vector (+ 3 current-opcode-index))
        next-vector (assoc vector output-position (* input1 input2))]
    (run (+ 4 current-opcode-index) next-vector)))

(defn run [current-opcode-index mem]
  (case (nth vector addr)
    1  (op-1 current-opcode-index mem)
    2  (op-2 current-opcode-index mem)
    99 (first vector)
    :something-wrong))
          
(= 19690720 (sm 0 real-input))

; p2
(def p2
  (for [x (range 0 99)
        y (range 0 99)]
    {:x x :y y :val (sm 0 (assoc (vec vec-input) 1 x 2 y))}))

(filter #(= 19690720 (:val %)) p2)
