(ns advent-of-code.d2-test
  (:require [clojure.test :refer :all]
            [advent-of-code.core :refer :all]))

(deftest test-1
  (is (= [2 0 0 0 99] (run 0 [1 0 0 0 99]))))
