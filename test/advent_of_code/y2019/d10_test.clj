(ns advent-of-code.y2019.d10-test
  (:require [clojure.test :refer :all]
            [advent_of_code.y2019.d10 :refer :all]))

(def small-map
".#..#
.....
#####
....#
...##"
)

(defn ->asteroid
  [map]
  (-> map
      (clojure.string/split-lines)
      (->stars)))

(deftest monitoring-system
  (let [asteroids (->asteroid small-map)
        detected (map  asteroids)]
    (is (= 7 (count-detected-stars [1 0] map)))))
