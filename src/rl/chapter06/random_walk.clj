;; gorilla-repl.fileformat = 1

;; @@
(ns rl.chapter06.random-walk
  (:require [incanter.stats :refer [sample-binomial]]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :refer [mean]]
            [rl.util :refer [take-until]])
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
(def init-states [0 0.5 0.5 0.5 0.5 0.5 1])
(def true-value [0 1/6 2/6 3/6 4/6 5/6 1])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter06.random-walk/true-value</span>","value":"#'rl.chapter06.random-walk/true-value"}
;; <=

;; @@
(defn gen-traces
  []
  (let [end? #(#{0 6} %)
        init-state 3
        move-left? #(= 1 (sample-binomial 1))
        next-state #(if (move-left?) (dec %) (inc %))]
    (take-until end? (iterate next-state init-state))))

(defn temporal-difference
  [states & {:keys [alpha batch]}]
  (let [alpha (or alpha 0.1)
        traces (gen-traces)
        states (if batch
                 states
                 (reduce
                  (fn [s [t0 t1]]
                    (update s t0 #(+ % (* alpha (- (s t1) (s t0))))))
                  states
                  (partition 2 1 traces)))
        rewards (repeat (dec (count traces)) 0)]
    [traces states rewards]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter06.random-walk/temporal-difference</span>","value":"#'rl.chapter06.random-walk/temporal-difference"}
;; <=

;; @@
(let [run #(second (temporal-difference %))
      states  (iterate run init-states)]
  (-> (plotly)
      (plot-seq
       (for [i [0 1 10 100]]
         #(add-scatter % :y (nth states i) :name (str i " episodes"))))
      (add-scatter :y true-value :name "true value")
      (plot "RL-figure-6-2-a" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/134.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
(defn monte-carlo
  [states & {:keys [alpha batch]}]
  (let [alpha (or alpha 0.1)
        traces (gen-traces)
        return (if (= 6 (last traces)) 1 0)
        states (if batch
                 states
                 (reduce
                  (fn [s t] (update s t #(+ % (* alpha (- return %)))))
                  states
                  traces))
        rewards (repeat (dec (count traces)) return)]
    [traces states rewards]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter06.random-walk/monte-carlo</span>","value":"#'rl.chapter06.random-walk/monte-carlo"}
;; <=

;; @@
(defn rmse
  [state]
  (m/sqrt (/ (m/esum (m/square (m/sub state true-value))) 5)))

(let [TD-alphas [0.15 0.1 0.05]
      MC-alphas [0.01 0.02 0.03 0.04]
      episodes 100
      n-round 100
      iter-TD (fn [alpha] (fn [state] (second (temporal-difference state :alpha alpha))))
      run-TD-once #(map rmse (take episodes (iterate (iter-TD %) init-states)))
      iter-MC (fn [alpha] (fn [state] (second (monte-carlo state :alpha alpha))))
      run-MC-once #(map rmse (take episodes (iterate (iter-MC %) init-states)))]
  (-> (plotly)
      (plot-seq
       (for [a TD-alphas]
         #(add-scatter %
                       :y (map mean (m/columns (for [_ (range n-round)] (run-TD-once a))))
                       :name (str "TD " a))))
      (plot-seq
       (for [a MC-alphas]
         #(add-scatter %
                       :y (map mean (m/columns (for [_ (range n-round)] (run-MC-once a))))
                       :name (str "MC " a))))
      (set-layout :yaxis {:showline true}
                  :xaxis {:showline true})
      (plot "RL-figure-6-2-b" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/136.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
(defn calc-updates
  "TODO: The MC method doesn't converge. Need to figure out why!"
  [update-type]
  (fn [[state traces rewards]]
    (let [alpha 0.001
          threshold 0.0012
          [t s r] (case update-type
                    :dt (temporal-difference state :batch true)
                    :mc (monte-carlo state :batch true))
          new-traces (conj traces t)
          new-rewards (conj rewards r)
          init-updates (m/zero-array [(count state)])
          updates (m/mul alpha
                         (reduce
                          (fn [us [ts rs]]
                            (case update-type
                              :dt (reduce (fn [u [[t0 t1] r]]
                                            (update u t0 #(+ % r (state t1) (- (state t0)))))
                                          us
                                          (map vector (partition 2 1 ts) rs))
                              :mc (reduce (fn [u [t r]]
                                            (update u t #(+ % r (- (state t)))))
                                          us
                                          (map vector ts rs))))
                          init-updates
                          (map vector new-traces new-rewards)))]
      (if (< (m/esum (m/abs updates)) threshold)
        [state new-traces new-rewards]
        (recur [(m/add state updates) traces rewards])))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter06.random-walk/calc-updates</span>","value":"#'rl.chapter06.random-walk/calc-updates"}
;; <=

;; @@
(defn td-rmse-batch
  []
  (let [calc-updates-TD (calc-updates :dt)]
    (map #(rmse (first %)) (take 100 (iterate calc-updates-TD [init-states [] []])))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter06.random-walk/td-rmse-batch</span>","value":"#'rl.chapter06.random-walk/td-rmse-batch"}
;; <=

;; @@
(-> (plotly)
    (add-scatter :y (map mean (m/columns (for [_ (range 100)] (td-rmse-batch)))))
    (plot "RL-figure-6-3" :fileopt "overwrite")
    embed-url)
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/138.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@

;; @@
