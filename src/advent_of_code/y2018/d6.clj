(ns advent_of_code.y2018.d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent_of_code.util :as u]))

(defn abs [n] (max n (- n)))

(def input
  (u/read-input "y2018/d6.input"))

(def coordinates
  (map (fn [s]
         (map #(Integer/parseInt %) (str/split s #", "))) input))

(def coordinates-map
  (zipmap (iterate inc 0) coordinates))

(def border
  (reduce (fn [acc itm]
            [(max (first acc) (first itm))
             (max (second acc) (second itm))])
          [0 0]
          coordinates))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(def grid
  (for [x (range 0 (inc (first border)))
        y (range 0 (inc (second border)))]
    {:coord [x y]
     :distances {}}))

(defn set-distances [grid [key target-coord]]
  (->> grid
       (map (fn [{:keys [coord distances]}]
              {:coord coord
               :distances (assoc distances key (distance target-coord coord))}))))

(def grid-with-distances
  (reduce (fn [acc itm]
            (set-distances acc itm))
          grid
          coordinates-map))

(defn set-value [{:keys [coord distances]}]
  (let [sorted-distances (->> distances
                              (sort-by second))
        f (first sorted-distances)
        s (second sorted-distances)
        value (if (= (second f) (second s))
                "."
                (str (first f)))]
    {:coord coord
     :value value}))

(def grid-with-value
  (->> grid-with-distances
       (map set-value)))

; visualize
(->> grid-with-value
     (sort-by (fn [{:keys [coord _]}]
                (second coord)))
     (map #(:value %))
     (partition (inc (first border)))
     (map #(str/join %)))


(def grid-aggregated-by-key
  (reduce (fn [acc itm]
            (let [{:keys [coord value]} itm]
              (if (= "." value)
                acc
                (if (get acc value)
                  (assoc acc value (conj (get acc value) coord))
                  (assoc acc value [coord])))))
          {}
          grid-with-value))

(defn infinite? [m]
  (let [max-x (first border)
        max-y (second border)
        xs (map first (second m))
        ys (map second (second m))]
    (or (some #(or (= 0 %) (= max-x %)) xs)
        (some #(or (= 0 %) (= max-y %)) ys))))

; p1
(->> grid-aggregated-by-key
     (filter #(not (infinite? %)))
     (map second)
     (map count)
     sort
     reverse
     first)

; p2
(defn set-total [{:keys [coord distances]}]
  (let [total (apply + (vals distances))]
    {:coord coord
     :total total}))

(def grid-with-total
  (map set-total grid-with-distances))

;; visualize
(->> grid-with-total
     (sort-by (fn [{:keys [coord _]}]
                (second coord)))
     (map #(:total %))
     (filter #(> 10000 %))
     (count))

(commnet
"00000.2222"
"00000.2222"
"0003342222"
"0033342222"
"..33344222"
"11.3444422"
"111.4444.."
"111.444555"
"111.445555"
"111.555555"

"........."
"........."
"........."
"...###..."
"..#####.."
"..#####.."
"...###..."
"........."
"........."
"........."

 )

