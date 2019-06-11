; --- Day 14: Chocolate Charts ---
; https://adventofcode.com/2018/day/14
(ns advent_of_code.y2018.d14)

(def target-num 793031)
(def target-vec [7 9 3 0 3 1])

(def initial
  {:elf1 0
   :elf2 1
   :sb [3 7]})

(defn next-sb
  [sb sum]
  (if (> sum 9)
    (conj sb 1 (rem sum 10))
    (conj sb sum)))

(defn next-pos
  [sb idx]
  (rem (+ 1 (get sb idx) idx) (count sb)))

(defn next-round
  [input]
  (let [{elf1 :elf1 elf2 :elf2 sb :sb} input
        sum (+ (get sb elf1) (get sb elf2))
        new-sb (next-sb sb sum)]
    {:elf1 (next-pos new-sb elf1)
     :elf2 (next-pos new-sb elf2)
     :sb new-sb}))

(defn until-target-round
  [target n input]
  (if (>= (count (:sb input)) (+ target n))
    input
    (recur target n (next-round input))))

(defn next-n-scores
  [target n input]
  (subvec (:sb (until-target-round target n input)) target (+ target n)))

; p1
(time (next-n-scores target-num 10 initial))

(defn until-target-vec
  [target n input]
  (if (= target (next-n-scores n (count target) input))
    n
    (recur target (inc n) (next-round input))))

; p2
(time (until-target-vec target-vec 0 initial))
