; --- Day 4: Repose Record ---
; https://adventofcode.com/2018/day/4
(ns advent_of_code.y2018.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as u]
            [clj-time.core :as t]))

(def input
  (u/read-input "y2018/d4.input"))

(defn split [s]
  (filter #(not= "" %) (str/split s #"\[|\]|\-|\:|\s")))

(defn parse [sa]
  (let [[year month day hour minute command gid] sa
        t (cond
            (= "Guard" command) :begin
            (= "falls" command) :sleep
            (= "wakes" command) :wake-up)
        gid (if (= :begin t)
              gid
              nil)]
    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :type t
     :gid gid}))

(defn add-time [m]
  (let [{:keys [year month day hour minute]} m
        time (t/date-time year month day hour minute)]
    (assoc m :time time)))

(defn aggregate-by-gid [data]
  (reduce (fn [acc itm]
            (let [{:keys [type gid time]} itm]
              (cond
                (= :begin type) (assoc acc :current-gid gid)
                :else (let [gid (:current-gid acc)]
                        (if (contains? acc gid)
                          (assoc acc gid (conj (get acc gid) time))
                          (assoc acc gid [time]))))))
          {:current-gid nil}
          data))

(def data
  (-> (->> input
           (map split)
           (map parse)
           (map add-time)
           (sort-by (juxt :year :month :day :hour :minute))
           aggregate-by-gid)
      (dissoc :current-gid)))

(defn total-slept [m]
  (let [[k v] m
        t (->> (partition 2 v)
               (map (fn [[start end]] (t/in-minutes (t/interval start end)))))]
    {:gid k
     :total (apply + t)}))

; the guard who spent the most minutes asleep
(->> (map total-slept data)
     (sort-by :total)) ; #2441

(comment
  
  )
