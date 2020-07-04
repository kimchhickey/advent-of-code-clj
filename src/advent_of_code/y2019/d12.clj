(ns advent-of-code.y2019.d12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def input
  (line-seq (io/reader (io/resource "y2019/d12.test.input"))))

(defn parse
  [s]
  (str/split (-> s
                 (str/replace #"\<x\=" "")
                 (str/replace #"\, y\=" ",")
                 (str/replace #"\, z\=" ",")
                 (str/replace #"\>" ""))
             #"\,"))

(defn ->pos
  [[x y z]]
  {:pos
   {:x (Integer/parseInt x)
    :y (Integer/parseInt y)
    :z (Integer/parseInt z)}
   :vel
   {:x 0
    :y 0
    :z 0}})

(def moons
  (->> input
       (map parse)
       (map ->pos)))

(defn gravity-single
  [source target k]
  (let [s (:pos source)
        t (:pos target)]
    (cond
      (= (k s) (k t)) 0
      (> (k s) (k t)) 1
      (< (k s) (k t)) -1)))

(defn apply-gravity
  [moon moons]
  (reduce
   (fn [moon other]
     (let [vx (:x (:vel moon))
           vy (:y (:vel moon))
           vz (:z (:vel moon))
           dx (gravity-single other moon :x)
           dy (gravity-single other moon :y)
           dz (gravity-single other moon :z)
           new-vel {:x (+ dx vx)
                    :y (+ dy vy)
                    :z (+ dz vz)}]
       (assoc moon :vel new-vel)))
   moon
   moons))

(defn apply-velocity
  [moon]
  (let [px (:x (:pos moon))
        py (:y (:pos moon))
        pz (:z (:pos moon))
        vx (:x (:vel moon))
        vy (:y (:vel moon))
        vz (:z (:vel moon))
        new-pos {:x (+ vx px)
                 :y (+ vy py)
                 :z (+ vz pz)}]
    (assoc moon :pos new-pos)))

(defn step
  [moons]
  (->> moons
       (map #(apply-gravity % moons))
       (map apply-velocity)))

(defn abs [n] (max n (- n)))

(defn energy
  [moon k]
  (let [x (abs (:x (k moon)))
        y (abs (:y (k moon)))
        z (abs (:z (k moon)))]
    (+ x y z)))

(defn pot-kin
  [moon]
  (assoc moon
         :pot (energy moon :pos)
         :kin (energy moon :vel)))

(defn total
  [moons]
  (->> moons
       (map pot-kin)
       (map #(* (:pot %) (:kin %)))
       (apply +)))

;; p2

(def mx
  [{:p -17 :v 0}
   {:p  -1 :v 0}
   {:p -19 :v 0}
   {:p  -6 :v 0}])

(def my
  [{:p  9 :v 0}
   {:p  7 :v 0}
   {:p 12 :v 0}
   {:p -6 :v 0}])

(def mz
  [{:p -5 :v 0}
   {:p 13 :v 0}
   {:p  5 :v 0}
   {:p -4 :v 0}])

(def mx-test
  [{:p -1 :v 0}
   {:p  2 :v 0}
   {:p  4 :v 0}
   {:p  3 :v 0}])

(def my-test
  [{:p   0 :v 0}
   {:p -10 :v 0}
   {:p  -8 :v 0}
   {:p   5 :v 0}])

(def mz-test
  [{:p  2 :v 0}
   {:p -7 :v 0}
   {:p  8 :v 0}
   {:p -1 :v 0}])

(defn gravity
  [mx mxs]
  (reduce (fn [acc v]
            (let [p1 (:p acc)
                  p2 (:p v)]
              (if (= p1 p2)
                acc
                (update acc :v (cond
                                  (> p1 p2) dec
                                  (< p1 p2) inc)))))
          mx
          mxs))

(defn pos
  [mx]
  (assoc mx :p (+ (:p mx) (:v mx))))

(defn mx->mx'
  [mx]
  (doall
   (->> mx
        (map #(gravity % mx))
        (map pos))))

(defn f
  [mx c target]
  (let [c'  (inc c)
        mx' (mx->mx' mx)]
    (if (= mx' target)
      c'
      (recur mx' c' target))))

(comment
  ;; test
  ;; x: 18
  ;; y: 28
  ;; z: 44
  (math/lcm 60424 (math/lcm 186028 231614))
)

(f mz 0 mz)

(first (drop 2771 (iterate mx->mx' mx-test)))

