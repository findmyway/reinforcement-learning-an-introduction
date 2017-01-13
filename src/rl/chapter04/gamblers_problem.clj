;; gorilla-repl.fileformat = 1

;; @@
(ns rl.chapter04.gamblers-problem
  (:require [clojure.core.matrix :as m]
            [incanter.core :refer [view sel dataset]]
            [incanter.charts :refer [line-chart add-categories]]
            [rl.util :refer [argmax]])
  (:use [plotly-clj.core]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(online-init)
;; @@
;; =>
;;; {"type":"html","content":"<script src=\"https://cdn.plot.ly/plotly-latest.min.js\" type=\"text/javascript\"></script>","value":"pr'ed value"}
;; <=

;; @@
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter04.gamblers-problem/find-converge-sv</span>","value":"#'rl.chapter04.gamblers-problem/find-converge-sv"}
;; <=

;; @@
(def state-value-converged (find-converge-sv init-statevalue))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter04.gamblers-problem/state-value-converged</span>","value":"#'rl.chapter04.gamblers-problem/state-value-converged"}
;; <=

;; @@
(let [idx (range 1 (inc (count init-statevalue)))
      s (iterate update-sv init-statevalue)]
  (-> (plotly {:x idx})
      (plot-seq
        (for [i [1 2 3 32]]
          #(add-scatter % :y (nth s i) :name (str "sweep " i))))
      (add-scatter :y state-value-converged :name "converged")
      (set-layout :xaxis {:title "Capital"}
                  :yaxis {:title "Value estimates"})
      (plot "RL-figure-4-3-a" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/116.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
(defn get-pl-elm
  [s sv]
  (let [actions (range (inc (min s (- goal s))))
        ac-returns (map #(+ (* head-prob (get sv (+ s %)))
                            (* (- 1 head-prob) (get sv (- s %))))
                        actions)]
    (get (vec actions) (ffirst (argmax ac-returns)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter04.gamblers-problem/get-pl-elm</span>","value":"#'rl.chapter04.gamblers-problem/get-pl-elm"}
;; <=

;; @@
(let  [pl (map #(get-pl-elm % state-value-converged) states)
       idx (range 1 (inc (count pl)))]
  (-> (plotly)
      (add-scatter :x idx :y pl :name "Final")
      (set-layout :xaxis {:title "Capital"}
                  :yaxis {:title "Final policy (stake)"})
      (plot "RL-figure-4-3-b" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/118.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@

;; @@
