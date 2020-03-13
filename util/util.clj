(ns advent_of_code.util)

(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn push-front [val length list]
  (if (<= length (count list))
    list
    (push-front val length (conj list val))))
