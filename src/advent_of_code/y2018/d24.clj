(ns advent-of-code.y2018.d24)

(def input
  {:immune-system [{:units      17
                    :hp         5390
                    :weak       #{:radiation :bludgeoning}
                    :attack     4507
                    :type       :fire
                    :initiative 2}
                   {:units      989
                    :hp         1274
                    :immune     #{:fire}
                    :weak       #{:bludgeoning :slashing}
                    :attack     25
                    :type       :slashing
                    :initiative 3}]
   :infection     [{:units      801
                    :hp         4706
                    :weak       #{:radiation}
                    :attack     116
                    :type       :bludgeoning
                    :initiative 1}
                   {:units      4485
                    :hp         2961
                    :immune     #{:radiation}
                    :weak       #{:fire :cold}
                    :attack     12
                    :type       :slashing
                    :initiative 4}]})

;; list<->map transformation
(defn ->list [armies]
  (concat (map #(assoc % :camp :immune-system) (:immune-system armies))
          (map #(assoc % :camp :infection) (:infection armies))))

(defn ->map [armies]
  (reduce
    (fn [res, item]
      (if (= :immune-system (:camp item))
        (assoc res :immune-system (conj (:immune-system res) item))
        (assoc res :infection (conj (:infection res) item))))
    {:immune-system []
     :infection []}
    armies))

;; target selection
(defn add-ep [army]
  (assoc army :ep (* (:units army) (:attack army))))

(defn selection-rule [v1 v2]
  (if (= (:ep v1) (:ep v2))
    (> (:initiative v1) (:initiative v2))
    (> (:ep v1) (:ep v2))))

(defn select-target [armies]
  (->> armies
       (->list)
       (map add-ep)
       (sort selection-rule)
       (select)))

;; attacking target
(defn attack [groups]
  (reduce
    (fn [result item]
      (update result :infection []))
    {:immune-system []
     :infection []}
    groups))

(defn attacking-rule [v1 v2]
  (> (:initiative v1) (:initiative v2)))

(defn attack-target [armies]
  (->> armies
       (sort attacking-rule)
       (attack)
       (->map)))


;; battle
(defn end? [armies]
  (or (empty? (:immune-system armies))
      (empty? (:infection armies))))

(defn round [armies]
  (->> armies
       (select-target)
       (attack-target)))

(defn battle [armies]
  (if (end? armies)
    armies
    (battle (round armies))))

;; run


;(defn effective-power
;  [group]
;  (* (:units group) (:attack group)))
;
;(defn selection-rule
;  [g1 g2]
;  (let [ep1 (effective-power g1)
;        ep2 (effective-power g2)]
;    (if (= ep1 ep2)
;      (> (:initiative g1) (:initiative g2))
;      (> ep1 ep2))))
;
;(defn sort-by-selection-rule
;  [armies]
;  (sort selection-rule armies))
;
;(defn fill-camp
;  [camp armies]
;  (map #(assoc % :camp camp) armies))
;
;(defn input->state
;  [input]
;  (sort-by-selection-rule (mapcat #(fill-camp % (% input)) (keys input))))
;
;(defn enemy
;  [army armies]
;  (filter (fn [candidate] (not= (:camp candidate) (:camp army))) armies))
;
;(defn predict-damage
;  [army enemy]
;  (if (contains? (:immune enemy) (:type army))
;    0
;    (if (contains? (:weak enemy) (:type army))
;      (* (effective-power army) 2)
;      (effective-power army))))
;
;(defn damage-prediction
;  [army enemies]
;  (map (fn [enemy] (assoc enemy :predicted-damage (predict-damage army enemy))) enemies))
;
;(defn target-selection
;  "Return the list of attacker->defender"
;  [army enemies]
;  (damage-prediction army enemies))
;
;(target-selection (first (:immune-system input)) (:infection input))
;
;(enemy (first (input->state input)) input)
;
;(map (fn [army] (target-selection army (enemy army input))) (input->state input))
;
;;(defn attacking
;;  "Return :immune-system and :infection after fighting."
;;  [armies]
;;  armies)
;;
;;(attacking (target-selection input))
