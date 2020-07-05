(ns advent_of_code.y2019.d10
  (:require [clojure.java.io :as io]
            [advent_of_code.util :as util]
            [clojure.algo.generic.math-functions :as generic-math]))

(def input
  (line-seq (io/reader (io/resource "y2019/d10.input"))))

(defn ->stars
  [input]
  (reduce 
   (fn [result row]
     (let [i (first row)
           js (->> (second row)
                   (map-indexed vector)
                   (filter #(= (second %) \#))
                   (map first))
           pairs (map vector js (iterate identity i))]
       (apply conj result pairs)
       ))
   []
   (map-indexed vector input)))

(def asteroids (->stars input))

(defn slope
  [[x1 y1]]
  (fn [[x2 y2]]
    (generic-math/atan2 (- y1 y2) (- x1 x2))))

(defn detect
  [all target]
  (let [slope-fn (slope target)]
    (->> all
         (filter #(not= target %))
         (map slope-fn)
         (into #{})
         (count))))

;; p1
(first (reverse (sort-by :count
                         (map (fn [ast]
                                {:ast ast
                                 :count (detect asteroids ast)}) asteroids)))) ;; [20, 21] + 247















(defn detect-all
  [all target]
  (let [slope-fn (slope target)]
    (->> all
         (filter #(not= target %))
         (map (fn [a]
                {:ast a
                 :slope (Math/toDegrees (slope-fn a))})))))


(def station
  (detect-all asteroids [20, 21]))

station




(defn ->degree
  [rad]
  (let [theta (Math/toDegrees rad)]
    (cond
      (and (> theta 0) (< theta 90)) (+ 270 theta)
      (= theta 90) 0
      (and (> theta 90) (<= theta 180)) (- theta 90)
      (< theta 0) (+ 90 (+ 180 theta)))))

(defn dist
  [[x1 y1]]
  (fn [[x2 y2]]
    (let [x (- x1 x2)
          y (- y1 y2)]
      (Math/sqrt (+ (* x x) (* y y))))))

(defn count-detected-stars
  [all target]
  (let [slope-fn (slope target)
        dist-fn (dist me)]
    (->> all
         (filter #(not= target %))
         (map (fn [c]
                (let [s (slope-fn c)]
                  {:coord c
                   :slope s
                   :degree (Math/toDegrees s)
                   :degree-360 (->degree s)
                   :dista (dist-fn c)}))))))

(defn count-with-coord
  [coord]
  {:coord coord
   :count (count (into #{}
                       (map :slope (count-detected-stars coord stars))))})

#_(first (reverse (sort-by
                 :count
                 (->> stars
                      (map count-with-coord)))))

(count (into #{} (map :slope (count-detected-stars [1, 0] stars))))

;; part 1
#_(first (reverse (sort (->> stars
                           (map #(count-detected-stars % stars))
                           (into #{})
                           (count)))))



;; solution using vector -> gift wrapping algorithms? -> no
;; part 2
(def station [20 21])

(defn ->polar-coordinate
  [station asteroids]
  (->> asteroids
       (map (fn [asteroid]
              (let [[x1 y1] station
                    [x2 y2] asteroid
                    dx (- x2 x1)
                    dy (- y2 y1)]
                {:p asteroid
                 :r (generic-math/atan2 dy dx)
                 :a 
                 (let [a (Math/toDegrees (generic-math/atan2 dy dx))
                       a' (+ 180 a)]
                   (if (<= 90 a')
                     (- a' 90)
                     (+ a' 270)))
                 :d (Math/sqrt (+ (* dx dx) (* dy dy)))})))))

(def ast
  {:s []
   :r (->> (->polar-coordinate station asteroids)
           (sort-by (juxt :a :d))
           (partition-by :a))})

(defn round
  [ast]
  (reduce
   (fn [acc v]
     (if (nil? v)
       acc
       {:s (conj (:s acc) (first v))
        :r (conj (:r acc) (rest v))}))
   {:s []
    :r []}
   ast))

(defn make-seq
  [ast]
  (let [{s :s r :r} ast
        {s' :s r' :r} (round r)
        s' (concat s s')
        r' (filter not-empty r')]
    (if (or (empty? r') (> (count s') 200))
      {:s s' :r r'}
      (recur {:s s' :r r'}))))

(first (drop 199 (map-indexed vector (:s (make-seq ast)))))

