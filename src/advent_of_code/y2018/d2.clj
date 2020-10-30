; --- Day 2: Inventory Management System ---
; https://adventofcode.com/2018/day/2
(ns advent_of_code.y2018.d2
  (:require [clojure.java.io :as io]))

(def input
  (line-seq (io/reader (io/resource "y2018/d2.input"))))


(def not-nil? (complement nil?))

;; part 1
(->> input
     (map frequencies)
     (map vals)
     (map #(into #{} %))
     (map #(map #{2 3} %))
     (map #(filter not-nil? %))
     flatten
     frequencies
     vals
     (apply *))

;; part 2
(defn diff [s1 s2]
  (reduce (fn [acc itm]
            (let [target (:target acc)
                  keys (:keys acc)]
              (if (= itm (first target))
                (update acc :target rest)
                (-> acc
                    (assoc :keys (conj keys itm))
                    (update :count inc)
                    (update :target rest)))))
          {:target s2
           :count 0
           :keys []}
          s1))

(def ans-set (reduce (fn [acc itm1]
                       (let [find-ans
                             (reduce (fn [acc itm2]
                                       (if (= 1 (:count (count-diff itm1 itm2)))
                                         (reduced #{itm1 itm2})
                                         []))
                                     []
                                     acc)]
                         (if (set? find-ans)
                           (reduced find-ans)
                           acc)))
                     input
                     input))
(def diff-char
  (first (:keys (diff (first ans-set)
                     (second ans-set)))))

(->> (first ans-set)
     (filter #(not= diff-char %))
     (apply str))

(comment
  ;; p1
  (def one (first input))
  (filter not-nil? (map #{2 3} (into #{} (vals (frequencies one)))))

  ;; p2
  (def two (second input))
  (count-diff one two)
  )

