(ns advent_of_code.y2018.d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as u]))

(def input
  (u/read-input "y2018/d7.sh.input"))

(defn ->extract-node [input]
  (let [extract (fn [s]
                  (let [ws     (str/split s #" ")
                        parent (keyword (nth ws 1))
                        child  (keyword (nth ws 7))]
                    [parent child]))]
    (map extract input)))

(def remaining-times
  (let [initial (+ 60 1)] ;; 0 -> 60
    (zipmap (map #(keyword (str %)) u/ALPHABET) (range initial (+ initial 26)))))

(defn ->dependency-graph [input]
  (reduce (fn [acc [parent child]]
            (let [old-parent (get acc parent)
                  old-child  (get acc child)
                  parent'    (if (nil? old-parent)
                               {:remaining-time (get remaining-times parent)
                                :parents        #{}}
                            old-parent)
                  child'     (if (nil? old-child)
                               {:remaining-time (get remaining-times child)
                                :parents        #{parent}}
                           (assoc old-child :parents (conj (:parents old-child) parent)))]
              (assoc acc parent parent' child child')))
          {}
          input))

(def todo
  (->> input
       ->extract-node
       ->dependency-graph))

(defn complete-jobs [todo doing done]
  (let [find-done-now     (fn [doing]
                        (->> doing
                             (filter (fn [[k m]] (= (:remaining-time m) 0)))
                             (map first)))
        remove-dependency (fn [todo done-now]
                            (->> todo
                                 (map (fn [[k m]]
                                        {k (assoc m :parents (apply disj (:parents m) done-now))}))
                                 (apply merge)))
        done-now          (find-done-now doing)
        todo'             (remove-dependency todo done-now)
        doing'            (apply dissoc doing done-now)
        done'             (apply conj done done-now)]
    [todo' doing' done']))

(defn do-jobs [doing sec]
  (let [doing' (->> doing
                    (map (fn [[k m]]
                           [k (update m :remaining-time dec)]))
                    (into {}))
        sec' (inc sec)]
    [doing' sec']))

(defn assign-jobs [todo doing concurrent-job-num]
  (let [next-job (->> todo
                      (filter #(empty? (:parents (second %))))
                      sort
                      first)]
    (if (or (>= (count doing) concurrent-job-num)
            (empty? todo)
            (nil? next-job))
      [todo doing]
      (let [doing' (into {} (conj doing next-job))
            todo'  (dissoc todo (first next-job))]
        (assign-jobs todo' doing' concurrent-job-num)))))

(defn ->next [state]
  (let [{:keys [concurrent-job-num
                sec
                todo
                doing
                done]}        state
        [doing' sec']         (do-jobs doing sec)
        [todo' doing'' done'] (complete-jobs todo doing' done)
        [todo'' doing''']     (assign-jobs todo' doing'' concurrent-job-num)]
    {:concurrent-job-num concurrent-job-num
     :sec                sec'
     :todo               todo''
     :doing              doing'''
     :done               done'}))

(defn run [state]
  (if (and (nil? (:todo state)) (empty? (:doing state)))
    state
    (run (->next state))))

(def state0-p1
  {:concurrent-job-num 1
   :sec -1
   :todo todo
   :doing {}
   :done []})

(def state0-p2
  {:concurrent-job-num 5
   :sec -1
   :todo todo
   :doing {}
   :done []})

; p1
(str/join (map #(str (name %)) (:done (run state0-p1))))

; p2
(:done (run state0-p2))
