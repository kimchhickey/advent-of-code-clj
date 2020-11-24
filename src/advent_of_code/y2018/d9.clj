(ns advent_of_code.y2018.d9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as util]
            [data.deque :as dq]))

(def initial-circle (dq/deque 0 1))

(defn rotate-cw [circle]
  (let [v (dq/peek-first circle)]
    (-> circle
        (dq/remove-first)
        (dq/add-last v))))

(defn rotate-ccw [circle]
  (let [v (dq/peek-last circle)]
    (-> circle
        (dq/remove-last)
        (dq/add-first v))))

(defn play-normal [state]
  (let [{:keys [circle num total-players current-player scores]} state
        circle' (-> circle
                    (rotate-cw)
                    (rotate-cw)
                    (dq/add-first num))
        num' (inc num)
        current-player' (rem (inc current-player) total-players)]
    {:circle circle'
     :num num'
     :total-players total-players
     :current-player current-player'
     :scores scores}))

(defn add [x]
  (fn [y]
    (+ x y)))

(defn play-23 [state]
  (let [{:keys [circle num total-players current-player scores]} state
        num' (inc num)
        rotated (-> circle
                    (rotate-ccw)
                    (rotate-ccw)
                    (rotate-ccw)
                    (rotate-ccw)
                    (rotate-ccw)
                    (rotate-ccw)
                    (rotate-ccw))
        value (dq/peek-first rotated)
        circle' (dq/remove-first rotated)
        current-player' (rem (inc current-player) total-players)
        new-value (+ num value)
        scores' (let [old-value (get scores current-player 0)]
                  (assoc scores current-player (+ old-value new-value)))]
    {:circle circle'
     :num num'
     :total-players total-players
     :current-player current-player'
     :scores scores'}))

(defn print-deque [deque]
  (let [v (dq/peek-first deque)]
    (if (nil? v)
      (do (prn))
      (do
        (print (str v " "))
        (recur (dq/remove-first deque))))))

(def initial-state
  {:circle initial-circle
   :num 2
   :total-players 9
   :current-player 1
   :scores {}})

(defn play [state]
  (let [num (:num state)]
    (if (= 0 (rem num 23))
      (play-23 state)
      (play-normal state))))

(comment
  ;; test cases
  (def state3 (play initial-state))
  (print-deque initial-circle)
  (print-deque (:circle state3))

  (def state4 (play state3))
  (prn state4)
  (print-deque (:circle state4))

  (def state22 (last (take 22 (iterate play initial-state))))
  (print-deque (:circle state22))

  (def state23 (last (take 23 (iterate play initial-state))))
  (prn state23)
  (print-deque (:circle state23))

  (def test-game-1
    {:circle initial-circle
     :num 2
     :current-player 1
     :total-players 10
     :scores {}})

  (def test-state-1 (last (take 1618 (iterate play test-game-1))))

  (def p1-game
    {:circle initial-circle
     :num 2
     :current-player 1
     :total-players 486
     :scores {}})

  (def p1-result
    (last (take 70833 (iterate play p1-game))))

  (def p2-result
    (time (last (take 7083300 (iterate play p1-game)))))

  (last (sort (vals (:scores p2-result))))
  )
