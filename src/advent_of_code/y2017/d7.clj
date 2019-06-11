(ns advent_of_code.y2017.d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cset]))

(def input
  (line-seq (io/reader (io/resource "y2017/d7.input"))))

(defn ->name
  [str]
  (first (str/split str #"\s\(")))

(defn ->weight
  [str]
  (Integer. (str/replace (last (str/split str #"\s\(")) #"\)" "")))

(defn ->tower
  ([program]
   {:name (->name program)
    :weight (->weight program)})
  ([program holdings]
   {:name (->name program)
    :weight (->weight program)
    :holdings (str/split holdings #",\s+")}))

(defn parse
  [line]
  (apply ->tower (str/split line #" -> ")))

(defn parent?
  [name holdings]
  (some #(= name %) holdings))

(defn find-parent
  [tower towers]
  (first (filter #(parent? (:name tower) (:holdings %)) towers)))

@(def towers (map parse input))

(defn find-bottom
  [tower towers]
  (let [parent (find-parent tower towers)]
    (if (empty? parent)
      tower
      (recur parent towers))))

;; p1
(find-bottom (first towers) towers)

;; p1 solved by Set Operation
(defn roots
  [coll]
  (into #{} (map :name coll)))

(defn leafs
  [coll]
  (into #{} (mapcat :holdings coll)))

(cset/difference
  (roots towers)
  (leafs towers))
