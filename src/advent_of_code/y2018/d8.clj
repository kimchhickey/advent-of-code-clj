(ns advent_of_code.y2018.d8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as u]))

(def input
  (let [input-str (->> "y2018/d8.input"
                       (u/read-input)
                       first)]
    (->> (str/split input-str #" ")
         (map #(Integer/parseInt %)))))

(def state0
  {:input input
   :node-stack [1]
   :metadata []
   :metadata-stack []})

(defn read-header [input]
  [(first input)
   (second input)
   (drop 2 input)])

(defn dec-stack [stack]
  (let [v (peek stack)]
    (if (= v 0)
      stack
      (conj (pop stack) (dec v)))))

(defn read-node [{:keys [input node-stack metadata metadata-stack]}]
  (let [[children-num metadata-num input'] (read-header input)
        node-stack' (conj (dec-stack node-stack) children-num)
        metadata-stack' (conj metadata-stack metadata-num)]
    {:input input'
     :node-stack node-stack'
     :metadata metadata
     :metadata-stack metadata-stack'}))

(defn read-metadata [{:keys [input node-stack metadata metadata-stack]}]
  (let [metadata-num (peek metadata-stack)
        metadata' (apply conj metadata (take metadata-num input))
        input' (drop metadata-num input)
        node-stack' (pop node-stack)]
    {:input input'
     :node-stack node-stack'
     :metadata metadata'
     :metadata-stack (pop metadata-stack)}))

(defn ->tree [state]
  (let [{:keys [input node-stack metadata-stack]} state
        _ (prn)]
    (if (empty? input)
      state
      (if (and (not (empty? node-stack))
               (= 0 (peek node-stack)))
        (->tree (read-metadata state))
        (->tree (read-node state))))))

(apply + (:metadata (->tree state0)))

(comment
  (def simple-case '(1 2 0 2 3 4 5 6)); [5 6] -> [3 4]
  (apply + [10 11 12 99 2 1 1 2])
  
  )
