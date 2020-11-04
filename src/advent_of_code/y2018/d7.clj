(ns advent_of_code.y2018.d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as u]))

(def input
  (u/read-input "y2018/d7.input"))

(defn parse [s]
  (let [ws (str/split s #" ")
        parent (keyword (nth ws 1))
        child (keyword (nth ws 7))]
    [parent child]))

(def parsed-input
  (map parse input))

(def letter-tree
  (reduce (fn [acc itm]
            (let [parent (first itm)
                  child (second itm)
                  old-parent (get acc parent)
                  old-child (get acc child)
                  parent' (if (nil? old-parent)
                            {:parents []
                             :children [child]}
                            (let [{:keys [parents children]} old-parent]
                              {:parents parents
                               :children (conj children child)}))
                  child' (if (nil? old-child)
                           {:parents [parent]
                            :childrent []}
                           (let [{:keys [parents children]} old-child]
                             {:parents (conj parents parent)
                              :children children}))]
              (assoc acc parent parent' child child')))
          {}
          parsed-input))

(defn find-next-key [letter-tree]
  (->> letter-tree
       (filter #(empty? (:parents (second %))))
       sort
       ffirst))

(defn do-it [letter-tree key]
  (->> letter-tree
       (filter #(not (= key (first %))))
       (map (fn [[k m]]
              (let [parents' (filter #(not (= key %)) (:parents m))]
                {k (assoc m :parents (into [] parents'))})))
       (apply merge)))

(defn step [letter-tree keys]
  (if (nil? letter-tree)
    keys
    (let [key' (find-next-key letter-tree)
          keys' (conj keys key')
          letter-tree' (do-it letter-tree key')]
      (recur letter-tree' keys'))))

; p1
(str/join (map name (step letter-tree [])))


; p2



