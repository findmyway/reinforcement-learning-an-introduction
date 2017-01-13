;; gorilla-repl.fileformat = 1

;; @@
(ns rl.chapter02.ten-armed-testbed
  (:require [incanter.stats :refer [sample-normal sample-binomial]]
            [rl.util :refer [argmax rand-weighted]]
            [clojure.core.matrix.stats :refer [mean]]
            [clojure.core.matrix :refer [to-nested-vectors mset!]]
            [clojure.core.matrix.operators :refer [+ - *]]
            [incanter.core :refer [view sel dataset]]
            [incanter.charts :refer [line-chart add-categories]])
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
(defn softmax
  [xs]
  (let [est (map #(Math/exp %) xs)
        est-sum (apply + est)
        est-norm (map #(/ % est-sum) est)]
    est-norm))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter02.ten-armed-testbed/softmax</span>","value":"#'rl.chapter02.ten-armed-testbed/softmax"}
;; <=

;; @@
(defn get-action
  [{:keys [k epsilon estimations ucb-degree action-counts times method]}]
  (cond
    (not (nil? ucb-degree)) (let [est (map #(+ %1 (* ucb-degree (Math/sqrt (/ (Math/log (inc times)) (inc %2)))))
                                           estimations action-counts)]
                              (-> est argmax shuffle ffirst))
    (= :gradient method) (rand-weighted (zipmap (range k) (softmax estimations)))
    :else (if (and (pos? epsilon) (= 1 (sample-binomial 1 :prob epsilon)))
            (rand-nth (range k))
            (-> estimations argmax shuffle ffirst))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter02.ten-armed-testbed/get-action</span>","value":"#'rl.chapter02.ten-armed-testbed/get-action"}
;; <=

;; @@
(defn get-reward!
  [action
   {:keys [means avg-reward action-counts times estimations method k step-size base-line] :as bandit}]
  (let [reward (sample-normal 1 :mean (nth means action))
        times (inc times)
        avg-reward (+ (/ reward times) (* avg-reward (/ (dec times) times)))
        action-counts (update action-counts action inc)
        estimations (case method
                      :sample-avg (update estimations action #(+ % (/ (- reward %) (nth action-counts action))))
                      :constant (update estimations action #(+ % (* (:step-size bandit) (- reward %))))
                      :gradient (let [a (update (vec (repeat k 0)) action inc)
                                      prob (softmax estimations)
                                      base (if base-line avg-reward 0)]
                                  (+ estimations (* step-size (- reward base) (- a prob))))
                      estimations)]
    (assoc! bandit
            :times times
            :avg-reward avg-reward
            :action-counts action-counts
            :estimations estimations)
    reward))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter02.ten-armed-testbed/get-reward!</span>","value":"#'rl.chapter02.ten-armed-testbed/get-reward!"}
;; <=

;; @@
;; data for figure-2-2-a
(defn rewards
  [steps & bandit-params]
  (let [wrapper (fn [b] (repeatedly steps #(get-reward! (get-action b) b)))
        bandits (for [_ (range 200)] (apply gen-bandit bandit-params))]
    (->> bandits
         (map wrapper)
         mean
         to-nested-vectors)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter02.ten-armed-testbed/rewards</span>","value":"#'rl.chapter02.ten-armed-testbed/rewards"}
;; <=

;; @@
(let [steps 1000 
      idx (range 1 (inc steps))]
  (-> (plotly)
      (add-scatter :x idx :y (rewards steps :epsilon 0) :name "$\\epsilon=0$")
      (add-scatter :x idx :y (rewards steps :epsilon 0.01) :name "$\\epsilon=0.01$")
      (add-scatter :x idx :y (rewards steps :epsilon 0.1) :name "$\\epsilon=0.1$")
      (set-layout 
        :title  "$\\text{Average performance of } \\epsilon \\text{ greedy action-value methods on the 10-armed testbed}$"
        :xaxis {:title "Step"}
        :yaxis {:title "Average reward"})
      (plot "RL-figure-2-2-a" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/100.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
;; data for figure-2-2-b
(defn actions
  [steps & bandit-params]
  (let [wrapper (fn [b]
                  (take steps (map #(if (= (:best-action b) %) 1 0)
                                   (iterate #(do (get-reward! % b) (get-action b))
                                            (get-action b)))))
        bandits (for [_ (range 200)] (apply gen-bandit bandit-params))]
    (->> bandits
         (map wrapper)
         mean
         to-nested-vectors)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter02.ten-armed-testbed/actions</span>","value":"#'rl.chapter02.ten-armed-testbed/actions"}
;; <=

;; @@
;; plot fitur-2-2-b
(let [steps 1000
      idx (range 1 (inc steps))]
  (-> (plotly)
      (add-scatter :x idx :y (actions steps :epsilon 0) :name "$\\epsilon=0$")
      (add-scatter :x idx :y (actions steps :epsilon 0.01) :name "$\\epsilon=0.01$")
  (add-scatter :x idx :y (actions steps :epsilon 0.1) :name "$\\epsilon=0.1$")
  (set-layout :xaxis {:title "Step"}
             :yaxis {:title "Optimal Action"}
             :title "$\\text{Average performance of } \\epsilon \\text{-greedy action-value methods on the 10-armed testbed}$")
  (plot "RL-figure-2-2-b" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/102.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
;; figure-2-3
(let [steps 1000
      idx (range 1 (inc steps))]
  (-> (plotly)
      (add-scatter :x idx
                   :y (actions steps :epsilon 0 :initial 5 :method :constant)
                   :name "$\\epsilon=0, initial=5$")
      (add-scatter :x idx
                   :y (actions steps :epsilon 0.1 :initial 0 :method :constant)
                   :name "$\\epsilon=0.1, initial=0$")
      (set-layout :xaxis {:title "Step"}
                  :yaxis {:title "Optimal action"})
      (plot "RL-figure-2-3" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/104.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
;; fiture-2-4
(let [steps 1000
      idx (range 1 (inc steps))]
  (-> (plotly)
      (add-scatter :x idx :y (rewards steps :ucb-degree 2) :name "UCB c=2")
      (add-scatter :x idx :y (rewards steps :epsilon 0.1) :name "$\\epsilon=0.1$")
      (set-layout :title "$\\text{Average performance of }\\epsilon \\text{-greedy action-value methods on the 10-armed testbed}$")
      (plot "RL-figure-2-4" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/106.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
(let [steps 1000
      idx (range 1 (inc steps))]
  (-> (plotly {:x idx})
      (add-scatter :y (actions steps :method :gradient :base-line true :step-size 0.1 :true-reward 4)
                   :name "base-line:true step-size:0.1")
      (add-scatter :y (actions steps :method :gradient :base-line true :step-size 0.4 :true-reward 4)
                   :name "base-line:true step-size:0.4")
      (add-scatter :y (actions steps :method :gradient :base-line false :step-size 0.1 :true-reward 4)
                   :name "base-line:false step-size:0.1")
      (add-scatter :y (actions steps :method :gradient :base-line false :step-size 0.4 :true-reward 4)
                   :name "base-line:false step-size:0.4")
      (plot "RL-figure-2-5" :fileopt "overwrite")
      embed-url))
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/108.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
(let [epsilon-greedy #(/ (apply + (rewards 1000 :method :sample-avg :epsilon %)) 1000)
      gradient #(/ (apply + (rewards 1000 :method :gradient :base-line true :step-size %)) 1000)
      ucb #(/ (apply + (rewards 1000 :ucb-degree %)) 1000)
      greedy-initial #(/ (apply + (rewards 1000 :epsilon 0 :initial %)) 1000)
      [xs1 xs2 xs3 xs4] (map #(map (fn [r] (Math/pow 2 r)) %)
                             [(range -7 -1) (range -5 2) (range -4 3) (range -2 3)])]
  (-> (plotly)
      (add-scatter :x xs1 :y (map epsilon-greedy xs1) :name "$\\epsilon greedy$")
      (add-scatter :x xs2 :y (map gradient xs2) :name "gradient bandit")
      (add-scatter :x xs3 :y (map ucb xs3) :name "UCB")
      (add-scatter :x xs4 :y (map greedy-initial xs4) :name "greedy with optimistic initialization")
      (set-layout :xaxis {:type "log" :title "$\\epsilon/\\alpha/c/Q_0$"})
      (plot "RL-figure-2-6" :fileopt "overwrite")
      embed-url))
 
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/110.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@

;; @@
