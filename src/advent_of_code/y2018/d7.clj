(ns advent_of_code.y2018.d7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn read-input [path]
  (-> path io/resource io/reader line-seq))

(defn parse-line
  [s]
  (some-> (re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." s)
          next))

(def input
  "입력을 [letter blocked-by] 인접 리스트 형태로 파싱. 예) [[:C :A] ...]"
  (->> (read-input "y2018/d7.input")
       (map parse-line)
       (map #(map keyword %))))

(defn make-time
  "알파벳 키워드 입력으로 걸리는 시간을 반환
   예) :A -> \"A\" -> \\A -> 65 -> 61"
  [k]
  (- (int (first (name k))) 4))

(defn make-initial-steps
  [l]
  (reduce
   (fn [steps letter]
     (assoc steps letter {:until      (make-time letter)
                          :now        0
                          :blocked-by #{}}))
   (sorted-map)
   (into #{} (flatten l))))

(defn make-steps
  "인접 리스트를 Step 노드 map으로 변경 생성하고,
   blocked-by에 의존성을 가진 step의 이름을 넣음
   최종 형태)
   {:A {:until ..
        :now ..
        :blocked-by #{...}}
    ... }"
  [l]
  (reduce
   (fn [result [blocked-by letter]]
     (update-in result [letter :blocked-by] #(conj % blocked-by)))
   (make-initial-steps l)
   l))

(defn take-workable-steps
  "todo 안에서 blocked-by가 비어있거나, 모두 done 처리된 step 만
   알파벳 순서로 정렬해서 n개 반환"
  [n todo done]
  (let [done-set  (->> done
                       (map first)
                       flatten
                       (into #{}))
        workable? (fn [[_ {:keys [blocked-by]}]]
                    (or (empty? blocked-by)
                        (= blocked-by (set/intersection blocked-by done-set))))]
    (->> todo
         (filter workable?)
         (into (sorted-map))
         (take n))))

(defn assign
  "doing 안의 step의 갯수가 worker-num 보다 작으면,
   todo에서 할일을 찾아서 todo에서 빼고 doing에 넣음"
  [worker-num todo doing done]
  (let [idle-worker-num (- worker-num (count doing))
        workable-steps  (take-workable-steps idle-worker-num todo done)
        todo'           (apply (partial dissoc todo) (keys workable-steps))
        doing'          (merge doing
                               workable-steps)]
    [todo' doing']))

(defn work
  "일하기. sec와 doing에 있는 step의 now를 +1초함."
  [sec doing]
  (let [doing' (reduce-kv #(assoc %1 %2 (update %3 :now inc)) {} doing)]
    [(inc sec) doing']))

(defn complete
  "일을 다 끝난 step을, doing에서 빼고, done에 넣음"
  [doing done]
  (let [complete? (fn [[_ v]] (= (:until v)
                                 (:now v)))
        doing'    (into {} (filter #(not (complete? %)) doing))
        done'     (->> doing
                       (filter complete?)
                       (reduce (fn [acc itm] (conj acc itm)) done))]
    [doing' done']))

(defn next-state
  "동시에 작업 가능한 수를 입력 받아서, 상태 변환 함수를 생성
   상태 변환 함수 : 이전 상태의 todo, doing, done을 받아서
                 1초 후의 todo, doing, done을 반환"
  [{worker-num :worker-num}]
  (fn [{:keys [sec todo doing done]
        :as   state}]
    (let [[todo doing] (assign worker-num todo doing done)
          [sec doing]  (work sec doing)
          [doing done] (complete doing done)]
      (assoc state
             :sec   sec
             :todo  todo
             :doing doing
             :done  done))))

(defn finished?
  [state]
  (and (empty? (:todo state))
       (empty? (:doing state))))

(def initial-state
  {:sec   0
   :todo  (make-steps input)
   :doing {}
   :done  []})

(def p1-solution
  (let [next-state-fn (next-state {:worker-num 1})
        end-state     (first (drop-while #(not (finished? %)) (iterate next-state-fn initial-state)))]
    (->> (:done end-state)
         (map first)
         (map name)
         (apply str))))

(def p2-solution
  (let [next-state-fn (next-state {:worker-num 5})
        end-state     (first (drop-while #(not (finished? %)) (iterate next-state-fn initial-state)))]
    (:sec end-state)))

(comment)
