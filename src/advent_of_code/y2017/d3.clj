(ns advent_of_code.y2017.d3)

(def input 325489)

(defn odd-numbers
  ([] (odd-numbers 1))
  ([n] (cons n (lazy-seq (odd-numbers (+ n 2))))))

(defn square [val] (* val val))

(first
  (->> (take 1000 (odd-square-numbers))
     (map square)
     (map-indexed vector)
     (filter #(< input (last %)))))

;; [285 326041]
