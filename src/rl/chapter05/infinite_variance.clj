(ns rl.chapter05.infinite-variance
  (:require [incanter.stats :refer [sample-binomial]]
            [clojure.core.matrix :as m]))

(defn behavior-policy [] (if (zero? (sample-binomial 1)) :end :back))
(defn target-policy [] :back)

(defn play
  []
  (loop [trajectory []]
    (let [a (behavior-policy)
          trajectory (conj trajectory a)]
      (cond
        (= a :end) [0 trajectory]
        (zero? (sample-binomial 1 :prob 0.9)) [1 trajectory]
        :else (recur trajectory)))))

(defn one-round
  [n-episodes]
  (let [get-ratio (fn [trajectory]
                    (if (= (last trajectory) :end)
                      0
                      (Math/pow 2 (count trajectory))))]
    (m/div (->> (repeatedly n-episodes play)
                (map (fn [[r t]] (* r (get-ratio t))))
                (reductions +))
           (range 1 (inc n-episodes)))))
