(ns rl.chapter06.cliff-walking
  (:require [incanter.stats :refer [sample-binomial]]
            [clojure.core.matrix :as m])
  (:use [debux.core]))

(def epsilon 0.1)
(def alpha 0.5)
(def gamma 1)

(def nrow 4)
(def ncol 12)
(def actions [[-1 0] ;; up
              [1 0]  ;; down
              [0 -1] ;; left
              [0 1]  ;; right
])
(def actions-idx (into {} (map-indexed #(vector %2 %1) actions)))

(def init-state-action-values (m/zero-array [nrow ncol (count actions)]))
(def start-state [3 0])
(def goal-state [3 11])
(def action-rewards
  (-> (m/zero-array [nrow ncol 4])
      (m/add -1)
      (m/set-indices (for [j (range 1 12)] [2 j 1]) -100)
      (m/set-selection 3 0 3 -100)))

(defn next-state
  [[row col] [row-a col-a]]
  (cond
    (and (= [3 0] [row col])
         (= [0 1] [row-a col-a]))
    start-state
    (and (= (= 2 row) (<= 1 col 10))
         (= [1 0] [row-a col-a]))
    start-state
    :else
    (let [new-row (-> (+ row row-a)
                      (min (dec nrow))
                      (max 0))
          new-col (-> (+ col col-a)
                      (min (dec ncol))
                      (max 0))]
      [new-row new-col])))

(def cache-state-action
  (into {}
        (for [i (range nrow) j (range ncol) k (range 4)
              :let [state [i j]
                    action (actions k)]]
          [[state action] (next-state state action)])))

(defn choose-action
  [[r c] sav]
  (if (sample-binomial 1 :prob epsilon)
    (rand-nth actions)
    (->> (ffirst (argmax (m/select sav r c :all)))
         int
         (nth actions))))
