(ns advent_of_code.y2019.d5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d2.input"))))

(def vec-input
  (let [opcodes (first input)]
    (map #(Integer/parseInt %) (str/split opcodes #","))))

(defn next [addr]
  (+ 4 addr))

(defn run [curr mem]
  (case (get mem curr)
    1 (let [input1 (get mem (get mem (+ 1 curr)))
            input2 (get mem  (get mem (+ 2 curr)))
            output-pos (get mem (+ 3 curr))
            next-mem (assoc mem output-pos (+ input1 input2))]
        (run (+ 4 curr) next-mem))
    2 (let [input1 (get mem (get mem (+ 1 curr)))
            input2 (get mem (get mem (+ 2 curr)))
            output-pos (get mem (+ 3 curr))
            next-mem (assoc mem output-pos (* input1 input2))]
        (run (+ 4 curr) next-mem))
    99 (first mem)
    :something-wrong))

(run 0 vec-input)
