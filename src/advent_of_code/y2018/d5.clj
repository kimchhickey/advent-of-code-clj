(ns advent_of_code.y2018.d5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as u]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def ALPHABET (str/upper-case alphabet))


(def input
  (first (u/read-input "y2018/d5.input")))

(def pairs (map (fn [c1 c2]
                  (str "(" c1 c2 ")|(" c2 c1 ")")) alphabet ALPHABET))

(def rgxq (str/join "|" pairs))

(defn react [s]
  (str/replace s (re-pattern rgx) ""))

; p1
(time
 (count (u/first-duplicate (iterate react input))))

; p2
(def units (map (fn [c1 c2] (str c1 "|" c2)) alphabet ALPHABET))

(defn remove-units [unit input]
  (str/replace input (re-pattern unit) ""))

(time
 (first (->> units
             (map #(remove-units % input))
             (map #(count (u/first-duplicate (iterate react %))))
             sort)))

(comment

  )
