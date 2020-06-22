(ns advent_of_code.y2019.intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]
            [clojure.math.combinatorics :as combo]))

(def program
  (let [code (-> "y2019/d9.input"
                       (io/resource)
                       (io/reader)
                       (line-seq)
                       (first))]
    (map #(Integer/parseInt %) (str/split code #","))))



(defn ->map
  [v]
  (into (sorted-map) (zipmap (iterate inc 0)
                      v)))

(defn ->opcode-and-mode
  [code]
  (let [s (vec (map #(Character/digit % 10) (take 5 (concat (reverse (str code)) (iterate identity \0)))))]
    {:opcode (Integer/parseInt (str (nth s 1) (nth s 0)))
     :mode-p1 (nth s 2)
     :mode-p2 (nth s 3)
     :mode-p3 (nth s 4)}))

(defn read-param
  [ip program mode rb]
  (let [value (get program ip)]
    (case mode
      ; 0 -  position mode
      0 (or (get program value) 0)
      ; 1 - immediate mode
      1 value
      ; 2 - relative mode
      2 (or (get program (+ rb value)) 0)
      )))

(defn read-param-default
  [ip program mode rb]
  (let [value (get program ip)]
    (case mode
      0 value
      1 value
      2 (+ rb value))))

(defn run
  [program ip in out rb]
  ; program : instructions vector
  ; ip      : instruction pointer
  ; in      : input value
  ; out     : output value
  (do
    #_(prn "--------")
    #_(prn {:program program
          :ip ip
          :in in
          :out out
          :rb rb})
    (let [code (get program ip)
          {:keys [opcode mode-p1 mode-p2 mode-p3]} (->opcode-and-mode code)]
      (do
        #_(prn {:code code
              :opcode opcode
              :mode-p1 mode-p1
              :mode-p2 mode-p2
              :mode-p3 mode-p3})
        (case opcode
          1 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  p2 (read-param (+ ip 2) program mode-p2 rb)
                  rst-pos (read-param-default (+ ip 3) program mode-p3 rb)
                  rst-val (+ p1 p2)
                  program' (assoc program rst-pos rst-val)
                  ip' (+ 4 ip)]
              (recur program' ip' in out rb))
          2 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  p2 (read-param (+ ip 2) program mode-p2 rb)
                  rst-pos (read-param-default (+ ip 3) program mode-p3 rb)
                  rst-val (* p1 p2)
                  program' (assoc program rst-pos rst-val)
                  ip' (+ 4 ip)]
              (recur program' ip' in out rb))
          3 (let [p1 (read-param-default (+ ip 1) program mode-p1 rb)
                  rst-val (first in)
                  in' (rest in)
                  program' (assoc program p1 rst-val)
                  ip' (+ 2 ip)]
              (recur program' ip' in' out rb))
          4 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  ip' (+ 2 ip)
                  out' (conj out p1)]
              #_{:status :doing
                 :program program
                 :ip ip'
                 :in in
                 :out rst-val}
              (recur program ip' in out' rb))
          5 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  p2 (read-param (+ ip 2) program mode-p2 rb)
                  ip' (if (not= 0 p1)
                        p2
                        (+ ip 3))]
              (recur program ip' in out rb))
          6 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  p2 (read-param (+ ip 2) program mode-p2 rb)
                  ip' (if (= 0 p1)
                        p2
                        (+ ip 3))]
              (recur program ip' in out rb))
          7 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  p2 (read-param (+ ip 2) program mode-p2 rb)
                  p3 (read-param-default (+ ip 3) program mode-p3 rb)
                  program' (if (< p1 p2)
                             (assoc program p3 1)
                             (assoc program p3 0))
                  ip' (+ 4 ip)]
              (recur program' ip' in out rb))
          8 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  p2 (read-param (+ ip 2) program mode-p2 rb)
                  p3 (read-param-default (+ ip 3) program mode-p3 rb)
                  program' (if (= p1 p2)
                             (assoc program p3 1)
                             (assoc program p3 0))
                  ip' (+ 4 ip)]
              (recur program' ip' in out rb))
          9 (let [p1 (read-param (+ ip 1) program mode-p1 rb)
                  rb' (+ rb p1)
                  ip' (+ 2 ip)]
              (recur program ip' in out rb'))
          99 {:status :finished
              :program program
              :ip ip
              :in in
              :out out
              :rb rb} ; should increase ip +1?
          )))))

(defn thruster-signal
  [program
   pss
   in]
  (do
    (prn {:program program
          :pss pss
          :in in})
    (if (empty? pss)
      in
      (let [fi (first pss)
            out (run program 0 [fi in] 0)]
        (thruster-signal program (rest pss) (:output out))))))

#_(let [program [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
               27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
      a (run program 0 [9 0] 0)
      b (run program 0 [8 (:out a)] 0)
      c (run program 0 [7 (:out b)] 0)
      d (run program 0 [6 (:out c)] 0)
      e (run program 0 [5 (:out d)] 0)]
  (feedback-loop a b c d e))

#_(defn d7p2
  [s]
  (let [a (run program 0 [(nth s 0) 0] 0)
        b (run program 0 [(nth s 1) (:out a)] 0)
        c (run program 0 [(nth s 2) (:out b)] 0)
        d (run program 0 [(nth s 3) (:out c)] 0)
        e (run program 0 [(nth s 4) (:out d)] 0)]
    (feedback-loop a b c d e)))

#_(first (reverse (sort-by :out (map d7p2 (combo/permutations [5 6 7 8 9])))))

(defn feedback-loop
  [a b c d e]
  (do
    (prn a)
    (prn b)
    (prn c)
    (prn d)
    (prn e)
    (let [a (run (:program a) (:ip a) [(:out e)] (:out a))
          b (run (:program b) (:ip b) [(:out a)] (:out b))
          c (run (:program c) (:ip c) [(:out b)] (:out c))
          d (run (:program d) (:ip d) [(:out c)] (:out d))
          e (run (:program e) (:ip e) [(:out d)] (:out e))]
      (if (= :finished (:status e))
        e
        (feedback-loop a b c d e)))))

#_(run [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 5 5 5] 18 [129] 0)

#_(first (reverse (sort-by :signal
                           (map (fn [s]
                                  {:permutation s
                                   :signal (thruster-signal program s 0)}) (combo/permutations [0 1 2 3 4])))))

;; day7 part 2
#_(:out (run (->map program) 0 [2] [] 0))

(comment
  ;; day9 p1
  (let [program [3,9,8,9,10,9,4,9,99,-1,8]
        program-map (vec->map program)]
    (run (= 1 (:out (run program-map 0 [8] 0 0)))))


  (let [program [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
        pss [4 3 2 1 0]]
    (thruster-signal program pss 0))


  (let [program [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                 101,5,23,23,1,24,23,23,4,23,99,0,0]
        pss [0,1,2,3,4]]
    (thruster-signal program pss 0))

  (run [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 0 [7] 0)

  (:output (run program 0 5 0))

  (run program 0 1 0)

  (let [program [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]]
    (run program 0 5324 0))

  (run 0 (assoc (vec input-vector) 1 12 2 2)) ; p1
  ;; TODO : replace find-first

  (filter #(= 19690720 (:output %))
          (for [noun (range 0 99)
                verb (range 0 99)]
            {:output (get (run 0 (assoc (vec input-vector) 1 noun 2 verb)) 0)
             :noun noun
             :verb verb}))) ; p2
  
