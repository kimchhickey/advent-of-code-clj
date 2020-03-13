(ns advent_of_code.y2019.d22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]))

(def input
  (line-seq (io/reader (io/resource "y2019/d22.input"))))

(take 10 input)

(defn read-last-num [s]
  (Integer/parseInt (last (str/split s #" "))))

(read-last-num "deal with increment 50")

(defn ->techniques [t]
  (if (str/includes? t "deal into new stack")
    {:technique :deal-into-new-stack}
    (let [number (read-last-num t)]
      (if (str/includes? t "deal with increment")
        {:technique :deal-with-increment-n :n number}
        {:technique :cut-n-cards :n number}))))

(def shuffle
  (map ->techniques input))

(take 10 shuffle)

(defn factory-ordered-deck [n]
  (let [n (range 0 n)]
    (reduce (fn [deck i]
              (assoc deck i i))
            {}
            n)))

(def initial-deck (factory-ordered-deck 10007))

; (take 10 initial-deck)

(defn deal-into-new-stack [deck]
  (into (sorted-map) (map-indexed (fn [idx itm] [idx itm]) (reverse (vals deck)))))

(deal-into-new-stack initial-deck)

(defn cut-n-cards [n deck]
  (let [deck (vals (into (sorted-map) deck))
        n (if (>= n 0) n (+ (count deck) n))
        left (take n deck)
        right (drop n deck)]
    (into (sorted-map) (map-indexed (fn [idx itm] [idx itm]) (concat right left)))))

(cut-n-cards 3 initial-deck)
(cut-n-cards -4 initial-deck)

(defn deal-with-increment-n [n deck]
  (let [size-of-deck (count deck)]
    (->> deck
         (reduce 
          (fn [deck card]
            (let [index     (first card)
                  value     (second card)
                  new-index (mod (* n index) size-of-deck)]
              (assoc deck new-index value)))
          {})
         (into (sorted-map)))))

; (into (sorted-map) (map-indexed (fn [idx itm] [idx itm]) (reverse (vals (deal-with-increment-n 3 initial-deck)))))



(deal-into-new-stack (deal-with-increment-n 3 initial-deck))

(cut-n-cards 3 (deal-with-increment-n 3 initial-deck))

(take 1 shuffle)

(defn run [command]
  (case (:technique command)
    :deal-into-new-stack ()))

; p1
(->> shuffle
     (reduce (fn [deck command]
               (case (:technique command)
                 :deal-into-new-stack (deal-into-new-stack deck)
                 :deal-with-increment-n (deal-with-increment-n (:n command) deck)
                 :cut-n-cards (cut-n-cards (:n command) deck)
                 deck)) initial-deck)
     (filter #(= (val %) 2019)))

; p2
; (def second-deck (factory-ordered-deck 119315717514047))

(->> shuffle
     (reduce (fn [deck command]
               (case (:technique command)
                 :deal-into-new-stack (deal-into-new-stack deck)
                 :deal-with-increment-n (deal-with-increment-n (:n command) deck)
                 :cut-n-cards (cut-n-cards (:n command) deck)
                 deck)) second-deck)
     (filter #(= (val %) 2019)))
