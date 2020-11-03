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

(comment

  )
