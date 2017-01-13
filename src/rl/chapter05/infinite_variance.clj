;; gorilla-repl.fileformat = 1

;; @@
(ns rl.chapter05.infinite-variance
  (:require [incanter.stats :refer [sample-binomial]]
            [clojure.core.matrix :as m])
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

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter05.infinite-variance/one-round</span>","value":"#'rl.chapter05.infinite-variance/one-round"}
;; <=

;; @@
(let [n 10000]
  (-> (plotly)
      (plot-seq
        (for [_ (range 10)]
          #(add-scatter % :x (range 1 (inc n)) :y (one-round n) :type "scattergl")))
      (set-layout :xaxis {:title "Episonde" :type "log"} :showlegend false)
      (plot "RL-figure-5-5" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/128.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@

;; @@
