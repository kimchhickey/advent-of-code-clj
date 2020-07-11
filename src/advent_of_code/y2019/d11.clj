(ns advent-of-code.y2019.d11)

(reduce
 (fn [acc v]
   (+ acc v))
 [1 2 3 4 5])

(defn -go
  [f & fs]
  (reduce
   (fn []
   f)
   fs)
)

(defn -pipe
  []
)

(defn -curry
  []
)


(def mt (map #(* % 2)))

(vec (mt [1 2 4 5 5]))
