(ns advent_of_code.y2019.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d5.input"))))

(def input-vector
  (let [opcodes (first input)]
    (map #(Integer/parseInt %) (str/split opcodes #","))))

(defn code-parser
  [code]
  (do 
    (prn code)
    (let [ss (map #(Character/digit % 10) (take 5 (concat (reverse (str code)) (iterate identity \0))))
          s (vec ss)
          r 
          {:opcode (Integer/parseInt (str (nth s 1) (nth s 0)))
           :first-param-mode (nth s 2)
           :second-param-mode (nth s 3)
           :third-param-mode (nth s 4)}]
      (do
        (prn r)
        r))))

(take 255 (count input-vector))

(defn read-parameter
  [index program mode]
  (let [value (nth program index)]
    (case mode
      0 ; position mode
      (nth program value)
      1 ; immediate mode
      value)))

(defn run
  [program index input-value output-value] ; index = instruction pointer
(do
  (prn program) 
  (let [code (nth program index)
        {:keys [opcode first-param-mode second-param-mode third-param-mode]} (code-parser code)]
    (case opcode ; opcode and parameters
      1 (let [first-param (read-parameter (+ index 1) program first-param-mode)
              second-param (read-parameter (+ index 2) program second-param-mode)
              result-position (nth program (+ index 3))
              result-value (+ first-param second-param)
              new-program (assoc program result-position result-value)
              new-index (+ 4 index)]
          (run new-program new-index input-value output-value))
      2 (let [first-param (read-parameter (+ index 1) program first-param-mode)
              second-param (read-parameter (+ index 2) program second-param-mode)
              result-position (nth program (+ index 3))
              result-value (* first-param second-param)
              new-program (assoc program result-position result-value)
              new-index (+ 4 index)]
          (run new-program new-index input-value output-value))
      3 (let [result-position (nth program (+ index 1))
              result-value input-value
              new-program (assoc program result-position result-value)
              new-index (+ 2 index)]
          (run new-program new-index input-value output-value))
      4 (let [result-value (read-parameter (+ index 1) program first-param-mode)
              new-index (+ 2 index)]
          (run program new-index input-value result-value))
      5 (let [first-param (read-parameter (+ index 1) program first-param-mode)
              second-param (read-parameter (+ index 2) program second-param-mode)
              new-index (if (not= 0 first-param) second-param (+ index 3))]
          (do
            (prn [first-param second-param new-index])
            (run program new-index input-value output-value)))
      6 (let [first-param (read-parameter (+ index 1) program first-param-mode)
              second-param (read-parameter (+ index 2) program second-param-mode)
              new-index (if (= 0 first-param) second-param (+ index 3))]
          (do
            (prn [first-param second-param])
            (run new-index program input-value output-value)))
      7 (let [first-param (read-parameter (+ index 1) program first-param-mode)
              second-param (read-parameter (+ index 2) program second-param-mode)
              third-param (nth program (+ index 3))
              new-program (if (< first-param second-param)
                            (assoc program third-param 1)
                            (assoc program third-param 0))
              new-index (+ 4 index)]
          (do
            (prn [first-param second-param third-param new-program])
            (run new-program new-index input-value output-value)))
      8 (let [first-param (read-parameter (+ index 1) program first-param-mode)
              second-param (read-parameter (+ index 2) program second-param-mode)
              third-param (nth program (+ index 3))
              new-program (if (= first-param second-param)
                            (do
                              (prn "1")
                              (prn [first-param second-param third-param])
                              (assoc program third-param 1))
                            (do
                              (prn "2")
                              (prn [first-param second-param third-param])
                              (assoc program third-param 0)))
              new-index (+ 4 index)]
          (run new-program new-index input-value output-value))
      99 {:output output-value
          :program program} ; should increase index +1?
      ))))

;; comment
(comment

(run [3,9,8,9,10,9,4,9,99,-1,8] 0 8 0)
(run 0 [3,9,7,9,10,9,4,9,99,-1,8] 8 0)
(run 0 [3,3,1108,-1,8,3,4,3,99] 19 0)
(run 0 [3,3,1107,-1,8,3,4,3,99] 8 0)

(run 0 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 1 0)

(run 0 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 0 0)

(run [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 0 7 0)

(run 0 (vec input-vector) 5 0)

  (run 0 input-vector 1 0)
  (take 10 input-vector)
  (run 0 [1 0 0 0 99])
  (run 0 (assoc (vec input-vector) 1 12 2 2)) ; p1
  ;; TODO : replace find-first
  (filter #(= 19690720 (:output %))
          (for [noun (range 0 99)
                   verb (range 0 99)]
            {:output (get (run 0 (assoc (vec input-vector) 1 noun 2 verb)) 0)
             :noun noun
             :verb verb})) ; p2
)
