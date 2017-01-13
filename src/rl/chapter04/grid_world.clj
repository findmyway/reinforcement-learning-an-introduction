;; gorilla-repl.fileformat = 1

;; @@
(ns rl.chapter04.grid-world
  (:require [clojure.core.matrix :as m])
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
(let [N 4
      reward -1
      world (m/zero-matrix N N)
      states (for [i (range N) j (range N)
                   :when (not (contains? #{[0 0] [(dec N) (dec N)]} [i j]))]
               [i j])
      actions [[-1 0] [1 0] [0 -1] [0 1]]  ;; [:left :right :down :up]
      prob (zipmap actions (repeat 0.25))
      update-v (fn [w idx]
                 (apply + (map #(* (prob %)
                                   (+ reward (get-in w (mapv + % idx) (get-in w idx))))
                               actions)))
      iter-fn #(mapv vec (partition N (concat [0] (map (partial update-v %) states) [0])))
      stop? (fn [[a b]] (< (m/abs (- (m/esum a) (m/esum b))) 0.0001))
      converge #(ffirst (filter stop? (partition 2 %)))]
  (-> (plotly)
      (add-surface :z (converge (iterate iter-fn world)))
      (plot "RL-figure-4-1" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/120.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@

;; @@
