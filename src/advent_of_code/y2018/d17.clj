(ns advent_of_code.y2018.d17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; 각각의 phase에 모든 tip position에 대해서
; vertical
;   #이 나올때까지 |로 채움, tip position은 그 끝
;
; horizontal
;   1. 양쪽이 뚫리면, |로 채움, tip position에 뚫린 두 곳의 위치를 저장
;   2. 한쪽이 뚫리면, |로 채움, tip position에 뚫린 한 곳의 위치를 저장
;   3. 양쪽다 막히면, ~로 채움, tip position은 원래 tip position의 y-1, tip position이 없으면,

(def input (line-seq (io/reader (io/resource "y2018/d17.input"))))

(defn parse-clay [s]
  (let [[_ axis v _ begin end] (re-find #"(\w)=(\d+), (\w)=(\d+)\.\.(\d+)" s)
        v (Integer/parseInt v)
        r (range (Integer/parseInt begin) (inc (Integer/parseInt end)))]
    (if (= axis "x")
      (map vector (repeat v) r)
      (map vector r (repeat v)))))

(->> input
     (map parse-clay))
