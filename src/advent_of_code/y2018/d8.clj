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
  {:last-index 1
   :input input
   :node-stack []
   :tree {}
   :metadata-stack []})

(defn read-header [input]
  [(first input)
   (second input)
   (drop 2 input)])

(defn update-child [new-child-key children]
  (conj children new-child-key))

(defn make-child [tree node-stack new-node-stack]
  ;; initial case
  (if (empty? node-stack)
    [tree node-stack]
    (let [[k v] (peek node-stack)]
      (if (= v 0)
        [tree node-stack]
        [(update-in tree [k :children] (partial update-child (first new-node-stack)))
         (conj (pop node-stack) [k (dec v)])]))))

(defn make-node [last-index children-num]
  [(inc last-index)
   [last-index children-num]
   {last-index {:children []
                :metadata []}}])

(defn read-node [{:keys [last-index input node-stack tree metadata-stack]}]
  (let [[children-num metadata-num input'] (read-header input)
        [last-index' new-node-stack new-tree-node] (make-node last-index children-num)
        [tree' node-stack'] (make-child tree node-stack new-node-stack)
        node-stack'' (conj node-stack' new-node-stack)
        metadata-stack' (conj metadata-stack metadata-num)
        tree'' (conj tree' new-tree-node)]
    {:last-index last-index'
     :input input'
     :node-stack node-stack''
     :tree tree''
     :metadata-stack metadata-stack'}))

(defn update-metadata [metadata v]
  (apply conj v metadata))

(defn read-metadata [{:keys [last-index input node-stack tree metadata-stack]}]
  (let [metadata-num (peek metadata-stack)
        metadata (take metadata-num input)
        [k _] (peek node-stack)
        tree' (update-in tree [k :metadata] (partial update-metadata metadata))
        input' (drop metadata-num input)
        node-stack' (pop node-stack)]
    {:last-index last-index
     :input input'
     :node-stack node-stack'
     :tree tree'
     :metadata-stack (pop metadata-stack)}))

(defn ->tree [state]
  (let [{:keys [input node-stack tree metadata-stack]} state
        _ (prn)]
    (if (empty? input)
      state
      ;; initial case
      (if (and (empty? node-stack)
               (empty? tree)
               (empty? metadata-stack))
        (read-node state)
        (if (and (not (empty? node-stack))
                 (= 0 (second (peek node-stack))))
          (read-metadata state)
          (read-node state))))))

(def parsed
  (u/first-duplicate (iterate ->tree state0)))

(defn value-of [[k v] m]
  (let [children (:children v)
        metadata (:metadata v)]
    (if (empty? children)
      (apply + metadata)
      (->> metadata
           (map (fn [i]
                  (let [child-key (get children (dec i))]
                    (if (nil? child-key)
                      0
                      (value-of [child-key
                                 (get m child-key)] m)))))
           (apply +)))))

(defn add-value [parsed]
  (map (fn [[k v]]
         [k (value-of [k v] parsed)]) parsed))

;; p2
(sort-by first (add-value (:tree parsed)))

(comment
  (def test
    {1 {:child-nodes [2 3]
        :metadata [1 1 2]}
     2 {:child-nodes []
        :metadata [10 11 12]}})

  (defn value-of []
    (apply + (map value-of child)))
  
  (def simple-case '(1 2 0 2 3 4 5 6)); [5 6] -> [3 4]
  (apply + [10 11 12 99 2 1 1 2])
  
  )
