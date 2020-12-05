(ns advent-of-code.y2020.d5
  (:require [advent_of_code.util :as util]))

(def input
  (util/read-input "y2020/d5.input"))

(def row-vec (range 0 128))
(def col-vec (range 0 8))

(defn search [o v]
  (if (empty? o)
    (first v)
    (let [cmd (first o)
          half (/ (count v) 2)
          o' (next o)]
      (if (or (= cmd \F) (= cmd \L))
        (recur o' (take half v))
        (recur o' (drop half v))))))

(defn parse-seat [s]
  (let [row-cmds (take 7 s)
        col-cmds (drop 7 s)])
  [(search row-cmds row-vec)
   (search col-cmds col-vec)])

(defn seat-id? [[r c]]
  (+ (* r 8) c))

;; p1
(->> input
     (map parse)
     (map seat-id?)
     sort
     last)

;; p2
(->> input
     (map parse)
     (sort)
     (partition-by #(identity (first %))))

(seat-id? [81 1])
