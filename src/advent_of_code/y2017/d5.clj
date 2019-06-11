(ns advent_of_code.y2017.d5
  (:require [clojure.java.io :as io]))

(def input
  (line-seq (io/reader (io/resource "y2017/d5.input"))))

(defn ->int
  [coll]
  (into [] (map #(Integer. %) coll)))

@(def maze
  (->int input))

(defn jump
  [state]
  (let [step (:step state)
        idx (:idx state)
        maze (:maze state)
        val (nth maze idx)]
    {:step (inc step) :idx (+ idx val) :maze (assoc maze idx (inc val))}))

(defn escape?
  [state]
  (>= (:idx state) (count (:maze state))))

(defn p1
  [state]
  (if (escape? state)
    (:step state)
    (recur (jump state))))

(p1 {:idx 0 :maze maze :step 0})

(defn jump-2
  [state]
  (let [step (:step state)
        idx (:idx state)
        maze (:maze state)
        val (nth maze idx)]
    {:step (inc step) :idx (+ idx val) :maze (assoc maze idx (inc val))}))

(defn p2
  [state]
  (if (escape? state)
    (:step state)
    (recur (jump-2 state))))

(p2 {:idx 0 :maze maze :step 0})
