(ns advent_of_code.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(def ALPHABET (str/upper-case alphabet))

(defn first-duplicate
  ([xs]
   (first-duplicate identity xs))
  ([key-fn xs]
   (let [result (reduce (fn [seen x]
                          (let [k (key-fn x)]
                            (if (seen k)
                              (reduced xn)
                              (conj seen k))))
                        #{} xs)]
     (if (set? result)
       nil
       result))))

(defn find-first [pred coll]
  (reduce (fn [acc itm]
            (if (pred itm)
              (reduced itm)
              acc))
          nil
          coll))
(defn read-input [path]
  (line-seq (io/reader (io/resource path))))
