(ns advent-of-code.y2019.intcode-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.y2019.intcode :refer :all]))

#_(deftest day2-part1
  (is (= [2 0 0 0 99]
         (run 0 [1 0 0 0 99])))
  (is (= [2 3 0 6 99]
         (run 0 [2 3 0 3 99])))
  (is (= [2 4 4 5 99 9801]
         (run 0 [2 4 4 5 99 0])))
  (is (= [30 1 1 4 2 5 6 0 99]
         (run 0 [1 1 1 4 99 5 6 0 99]))))

#_(deftest day5-part1
  (is (= [1101 100 -1 4 99]
         (run 0 [1101 100 -1 4 0])))
  (is (= [1002 4 3 4 99]
         (run 0 [1002 4 3 4 33])))
  (is (= [3 0 4 0 99]
         (run 0 [3 0 4 0 99])))
  #_(is (= [3 9 8 9 10 9 4 9 99 -1 8]
         (run 0 [3 9 8 9 10 9 4 9 99 -1 8]))))

(deftest day5-part1
  #_(let [file (line-seq (io/reader (io/resource "y2019/d5.input")))
        strs (str/split (first file) #",")
        input (map #(Integer/parseInt %) strs)
        program (vec input)]
    (testing "p1"
      (is (= (:output (run program 0 1 0))
             13933662)))))

(deftest day5-part2
  (let [ip 0 ; initial instruction pointer
        in 0
        out 0]
    (testing "comparison"
      (testing "opcode 3, 8, 4 using position mode"
        (let [program [3,9,8,9,10,9,4,9,99,-1,8]]
          (is (= 1 (:output (run program ip 8 out))))
          (is (= 0 (:output (run program ip 7 out))))
          (is (= 0 (:output (run program ip 9 out))))))
      (testing "opcode 3, 7, 4 using position mode"
        (let [program [3,9,7,9,10,9,4,9,99,-1,8]]
          (is (= 0 (:output (run program ip 8 out))))
          (is (= 0 (:output (run program ip 9 out))))
          (is (= 1 (:output (run program ip 7 out))))))
      (testing "opcode 3, 8, 4 using immediate mode"
        (let [program [3,3,1108,-1,8,3,4,3,99]]
          (is (= 1 (:output (run program ip 8 out))))
          (is (= 0 (:output (run program ip 7 out))))
          (is (= 0 (:output (run program ip 9 out))))))
      (testing "opcode 3, 7, 4 using immediate mode"
        (let [program [3,3,1107,-1,8,3,4,3,99]]
          (is (= 0 (:output (run program ip 8 out))))
          (is (= 1 (:output (run program ip 7 out))))
          (is (= 0 (:output (run program ip 9 out)))))))
    (testing "jump"
      (testing "using position mode"
        (let [program [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]]
          (is (= 0 (:output (run program ip 0 out))))
          (is (not= 0 (:output (run program ip 1 out))))))
      (testing "using immediate mode"
        (let [program [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]]
          (is (= 0 (:output (run program ip 0 out))))
          (is (not= 0 (:output (run program ip 1 out)))))))
    (testing "large"
      (testing ""
        (let [program [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]]
          (is (= 999 (:output (run program ip 7 out))))
          (is (= 1000 (:output (run program ip 8 out))))
          (is (= 1001 (:output (run program ip 9 out)))))))))
