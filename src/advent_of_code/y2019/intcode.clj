(ns advent_of_code.y2019.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d5.input"))))

(def vec-input
  (let [opcodes (first input)]
    (map #(Integer/parseInt %) (str/split opcodes #","))))

; (take 10 vec-input)

(def initial-input 1)

(defn store-at [addr val mem]
  (assoc mem addr val))

(defn apply-mode [mode p mem]
  (case mode
    ; position mode
    0 (get mem (get mem p))
    ; immediate mode
    1 (get mem p)))

(defn make-op [f]
  (fn [m1 m2 m3 p mem]
    (let [v1 (apply-mode m1 (+ 1 p) mem)
          v2 (apply-mode m2 (+ 2 p) mem)
          addr (apply-mode m3 (+ 3 p) mem)
          res (f v1 v2)]
      (store-at addr res mem))))

(def op-01 (make-op +))
(def op-02 (make-op *))

(defn push-front-zero [list]
  (util/push-front 0 5 list))

(defn l->opcode [param]
  (let [modes (take 3 param)
        opcode (+ (nth param 3) (nth param 4))]
    {:modes (reverse modes)
     :opcode opcode}))

(defn ->param [code]
  (-> code
      util/digits
      push-front-zero
      l->opcode))

(def context (atom {:input 1 :output 0}))

(defn op-03 [p mem]
  (let [addr (get mem (+ 1 p))]
    (store-at addr (:input @context) mem)))

(defn op-04 [p mem]
  (let [val (get mem (get mem (+ 1 p)))
        update (swap! context assoc :output val)]
    mem))

(defn run [curr mem]
  (let [params (->param (get mem curr))]
    (case (:opcode params)
      1  (let [new-mem (op-01 (:modes params) curr mem)]
           (run (+ 4 curr) new-mem))
      2  (let [new-mem (op-02 (:modes params) curr mem)]
           (run (+ 4 curr) new-mem))
      3  (let [new-mem (op-03 curr mem)]
           (run (+ 2 curr) new-mem))
      4  (let [new-mem (op-04 curr mem)]
           (run (+ 2 curr) new-mem))
      99 (first mem)
      :something-wrong)))

(take 10 vec-input)

(run 0 [1002 4 3 4 33])

; p2
(def p2
  (for [x (range 0 99)
        y (range 0 99)]
    {:x x :y y :val (run 0 (assoc (vec vec-input) 1 x 2 y))}))

(filter #(= 19690720 (:val %)) p2)
