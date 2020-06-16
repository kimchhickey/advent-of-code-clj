(ns advent_of_code.y2019.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def program
  (let [code (-> "y2019/d5.input"
                       (io/resource)
                       (io/reader)
                       (line-seq)
                       (first))]
    (vec (map #(Integer/parseInt %) (str/split code #",")))))

(defn ->opcode-and-mode
  [code]
  (let [s (vec (map #(Character/digit % 10) (take 5 (concat (reverse (str code)) (iterate identity \0)))))] 
    {:opcode (Integer/parseInt (str (nth s 1) (nth s 0)))
     :mode-p1 (nth s 2)
     :mode-p2 (nth s 3)
     :mode-p3 (nth s 4)}))

(defn read-param
  [ip program mode]
  (let [value (nth program ip)]
    (case mode
      ; 0 -  position mode
      0 (nth program value)
      ; 1 - immediate mode
      1 value)))

(defn run
  [program ip in out]
  ; program : instructions vector
  ; ip      : instruction pointer
  ; in      : input value
  ; out     : output value
  (let [code (nth program ip)
        {:keys [opcode mode-p1 mode-p2 mode-p3]} (->opcode-and-mode code)]
    (case opcode ; opcode and parameters
      1 (let [p1 (read-param (+ ip 1) program mode-p1)
              p2 (read-param (+ ip 2) program mode-p2)
              rst-pos (nth program (+ ip 3))
              rst-val (+ p1 p2)
              program' (assoc program rst-pos rst-val)
              ip' (+ 4 ip)]
          (run program' ip' in out))
      2 (let [p1 (read-param (+ ip 1) program mode-p1)
              p2 (read-param (+ ip 2) program mode-p2)
              rst-pos (nth program (+ ip 3))
              rst-val (* p1 p2)
              program' (assoc program rst-pos rst-val)
              ip' (+ 4 ip)]
          (run program' ip' in out))
      3 (let [rst-pos (nth program (+ ip 1))
              rst-val in
              program' (assoc program rst-pos rst-val)
              ip' (+ 2 ip)]
          (run program' ip' in out))
      4 (let [rst-val (read-param (+ ip 1) program mode-p1)
              ip' (+ 2 ip)]
          (run program ip' in rst-val))
      5 (let [p1 (read-param (+ ip 1) program mode-p1)
              p2 (read-param (+ ip 2) program mode-p2)
              ip' (if (not= 0 p1)
                       p2
                       (+ ip 3))]
          (run program ip' in out))
      6 (let [p1 (read-param (+ ip 1) program mode-p1)
              p2 (read-param (+ ip 2) program mode-p2)
              ip' (if (= 0 p1)
                       p2
                       (+ ip 3))]
          (run program ip' in out))
      7 (let [p1 (read-param (+ ip 1) program mode-p1)
              p2 (read-param (+ ip 2) program mode-p2)
              p3 (nth program (+ ip 3))
              program' (if (< p1 p2)
                         (assoc program p3 1)
                         (assoc program p3 0))
              ip' (+ 4 ip)]
          (run program' ip' in out))
      8 (let [p1 (read-param (+ ip 1) program mode-p1)
              p2 (read-param (+ ip 2) program mode-p2)
              p3 (nth program (+ ip 3))
              program' (if (= p1 p2)
                         (assoc program p3 1)
                         (assoc program p3 0))
              ip' (+ 4 ip)]
          (run program' ip' in out))
      99 {:output out
          :program program} ; should increase ip +1?
      )))

(comment
  (run [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 0 7 0)

  (:output (run program 0 5 0))

  (run program 0 1 0)

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
