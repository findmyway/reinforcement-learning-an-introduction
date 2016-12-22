(ns rl.chapter04.gamblers-problem
  (:require [clojure.core.matrix :as m]
            [incanter.core :refer [view sel dataset]]
            [incanter.charts :refer [line-chart add-categories]]
            [rl.util :refer [argmax]]))

(def goal 100)
(def states (range (inc goal)))
(def head-prob 0.4)
(def init-policy (m/zero-array [(inc goal)]))
(def init-statevalue (assoc (m/zero-array [(inc goal)]) goal 1))

(defn update-sv-ele
  [s sv]
  (let [actions (range (inc (min s (- goal s))))
        ac-returns (map #(+ (* head-prob (get sv (+ s %)))
                            (* (- 1 head-prob) (get sv (- s %))))
                        actions)]
    (apply max ac-returns)))

(defn update-sv
  [sv]
  (mapv #(update-sv-ele % sv) states))

(defn converge?
  [[a b]]
  (< (m/abs (- (m/esum a) (m/esum b)))
     1.0e-10))

(defn find-converge-sv
  [sv]
  (ffirst (filter converge? (partition 2 (iterate update-sv sv)))))

(def state-value-converged (find-converge-sv init-statevalue))

(let [idx (range 1 (inc (count init-statevalue)))
      s (iterate update-sv init-statevalue)]
  (doto (line-chart idx (nth s 1))
    (add-categories idx (nth s 2))
    (add-categories idx (nth s 3))
    (add-categories idx (nth s 32))
    (add-categories idx state-value-converged)
    view))

(defn get-pl-elm
  [s sv]
  (let [actions (range (inc (min s (- goal s))))
        ac-returns (map #(+ (* head-prob (get sv (+ s %)))
                            (* (- 1 head-prob) (get sv (- s %))))
                        actions)]
    (get (vec actions) (ffirst (argmax ac-returns)))))

(let  [pl (map #(get-pl-elm % state-value-converged) states)
       idx (range 1 (inc (count pl)))]
  (doto (line-chart idx pl)
    view))
