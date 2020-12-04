(ns advent_of_code.y2020.d4
  (:require [advent_of_code.util :as util]
            [clojure.string :as string]))

(def input (util/read-input "y2020/d4.input"))
(def test-input (util/read-input "y2020/d4.test.input"))
(def valid-input (util/read-input "y2020/d4.valid.input"))
(def invalid-input (util/read-input "y2020/d4.invalid.input"))

(defn parse [input]
  (->> input
       (partition-by #(= "" %))
       (filter #(not (empty? (first %))))
       (map #(string/join " " %))
       (map #(string/split % #":| "))
       (map #(partition 2 %))
       (map #(map (fn [[k v]]
                    [(keyword k) v]) %))
       (map #(into (sorted-map) %))))

(defn all-keys? [m]
  (= 7 (count (keys (dissoc m :cid)))))

;; p1
(->> (parse input)
     (map #(all-keys? %))
     (filter #(= true %))
     (count))

;; p2
(defn valid? [m]
  (if (all-keys? m)
    (let [{:keys [byr iyr eyr hgt hcl ecl pid cid]} m
          byr' (<= 1920 (Long/parseLong byr) 2002)
          iyr' (<= 2010 (Long/parseLong iyr) 2020)
          eyr' (<= 2020 (Long/parseLong eyr) 2030)
          hgt' (let [[_ hgt-val hgt-unit] (re-find #"(\d+)(.+)" hgt)]
                 (if (= "cm" hgt-unit)
                   (<= 150 (Long/parseLong hgt-val) 193)
                   (<= 59 (Long/parseLong hgt-val) 76)))
          hcl' (not (nil? (re-matches #"#[A-Za-z0-9]{6}" hcl)))
          ecl' (let [colors #{:amb :blu :brn :gry :grn :hzl :oth}]
                 (contains? colors (keyword ecl)))
          pid' (not (nil? (re-matches #"[0-9]{9}" pid)))]
      (and byr' iyr' eyr' hgt' hcl' ecl' pid'))
    false))

(->> (parse input)
     (map valid?)
     (filter #(= true %))
     (count))
