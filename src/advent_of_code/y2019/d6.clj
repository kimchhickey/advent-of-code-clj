(ns advent_of_code.y2019.d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d6.input"))))

(defn parse [s]
  (map keyword (str/split s #"\)")))

(defn ->orbits [space r]
  (let [object (first r)
        satellite (second r)]
   (if (contains? space object)
     (assoc space object (conj (get space object) satellite))
     (assoc space object (vector satellite)))))

(def orbit-graph
  (let [edges (map parse input)
        bi-edges (mapcat (juxt identity reverse) edges)] ;; for p2
    (->> bi-edges
         (reduce ->orbits {}))))

(def bi-input
  (let [i (map parse input)]
    (concat i (map reverse i))))

;; p1
(def count-the-total-number-of-orbits
  (->> (bfs orbit-graph :COM)
       (map second)
       (reduce +)))

;; p2
(def count-san-from-you
  (->> (bfs orbit-graph :YOU)
       (filter #(= :SAN (first %)))))

;; graph utils
(defn bfs
  [graph v]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [v 0])
         visited []]
    (if (empty? queue)
      visited
      (let [[v depth] (peek queue)
            neighbors (get graph v)
            nodes (map first visited)
            not-visited (filter #(not (lazy-contains? nodes %)) neighbors)
            new-queue (apply conj (pop queue) (partition 2 (interleave not-visited (iterate identity (inc depth)))))]
        (if (lazy-contains? nodes v)
          (recur new-queue visited)
          (recur new-queue (conj visited [v depth])))))))

(defn lazy-contains? [col key]
  (some #{key} col))

