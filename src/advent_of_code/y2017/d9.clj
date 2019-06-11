(ns advent_of_code.y2017.d9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader (io/resource "y2017/d9.input"))))

; remove ignore characters
@(def ignored
  (str/replace (first input) #"(\!.)" ""))

; remove garbage characters
@(def groups
  (str/replace ignored #"(\<.*?\>)" ""))

; p1
(reduce
  (fn [result item]
    (let [{score :score total :total} result]
      (if (= item \{)
        {:score (inc score) :total (+ score total)}
        (if (= item \})
          {:score (dec score) :total total}
          result))))
  {:score 1 :total 0}
  groups)

; p2
(defn toggle-garbage
  [result]
  (update result :garbage? #(if (= % true) false true)))

(defn inc-total
  [result]
  (update result :total inc))

(reduce
  (fn [result item]
    (if (= (:garbage? result) true)
      (if (= item \>)
        (toggle-garbage result)
        (inc-total result))
      (if (= item \<)
        (toggle-garbage result)
        result)))
  {:garbage? false :total 0}
  ignored)
