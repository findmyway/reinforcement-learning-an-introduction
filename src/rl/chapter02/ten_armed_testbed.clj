;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here we use the ggplot library in R to plot figures-2-1.
;;; It means that you should install R and ggplot first.
;;; 
;;; [Install R]:https://www.r-project.org/
;;; [Install ggplot]:http://ggplot2.org/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns rl.chapter02.ten-armed-testbed
  (:require [incanter.stats :refer [sample-normal sample-binomial]]
            [gg4clj.core :as gg]
            [rl.util :refer [argmax rand-weighted]]
            [clojure.core.matrix.stats :refer [mean]]
            [clojure.core.matrix :refer [to-nested-vectors mset!]]
            [clojure.core.matrix.operators :refer [+ - *]]
            [incanter.core :refer [view sel dataset]]
            [incanter.charts :refer [line-chart add-categories]]))

(defn fig-2-1
  []
  (let [K 10  ;; k-armed
        C 200 ;; C samples for each bandit
        idx (range 1 11)
        means (sample-normal K)
        rewards (mapcat #(sample-normal C :mean %) means)
        def-df [:<- :g (gg/data-frame {:Action (mapcat #(repeat 200 %) idx) :Reward rewards})]
        def-aes-data [:<- :l (gg/data-frame {:lineXstart (map #(- % 0.2) idx)
                                             :lineXend (map #(+ % 0.2) idx)
                                             :lineY means
                                             :label (map #(format "q[\\'*\\'](%d)" %) idx)})]
        graph (gg/r+
               [:ggplot :g [:aes {:x [:factor :Action]
                                  :y :Reward
                                  :width 0.4}]]
               [:geom_violin {:trim :FALSE
                              :scale "width"}]
               [:geom_segment {:data :l
                               :mapping [:aes {:x :lineXstart :xend :lineXend :y :lineY :yend :lineY}]}]
               [:geom_text {:data :l
                            :mapping [:aes {:x :lineXend :y :lineY :label :label}]
                            :hjust "left"
                            :parse "TRUE"
                            :size 2}])
        cmds [def-df def-aes-data graph]]
    (let [img-name "resources/figures/figure-2-1.pdf"]
      (gg/gg-save cmds img-name))))

(defn gen-bandit
  [& {:keys [k epsilon initial method step-size ucb-degree base-line true-reward]
      :or {k 10 epsilon 0 initial 0 method :sample-avg step-size 0.1 true-reward 0}}]
  (let [means (sample-normal k :mean true-reward)]
    (transient {:k k
                :epsilon epsilon
                :ucb-degree ucb-degree
                :means means
                :best-action (ffirst (argmax means))
                :estimations (vec (repeat k initial))
                :action-counts (vec (repeat k 0))
                :avg-reward 0
                :method method
                :step-size step-size
                :base-line base-line
                :times 0})))
(defn softmax
  [xs]
  (let [est (map #(Math/exp %) xs)
        est-sum (apply + est)
        est-norm (map #(/ % est-sum) est)]
    est-norm))

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

;; data for figure-2-2-a
(defn rewards
  [steps & bandit-params]
  (let [wrapper (fn [b] (repeatedly steps #(get-reward! (get-action b) b)))
        bandits (for [_ (range 200)] (apply gen-bandit bandit-params))]
    (->> bandits
         (map wrapper)
         mean
         to-nested-vectors)))

;; plot fiture-2-2-a
(let [steps 1000
      idx (range 1 (inc steps))]
  (doto (line-chart idx (rewards steps :epsilon 0)
                    :series-label "epsilon: 0"
                    :legend true
                    :x-label "Step"
                    :y-label "Average reward"
                    :title "Average performance of ε-greedy action-value methods on the 10-armed testbed")
    (add-categories idx (rewards steps :epsilon 0.01) :series-label "epsilon: 0.01")
    (add-categories idx (rewards steps :epsilon 0.1) :series-label "epsilon: 0.1")
    view))

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

;; plot fitur-2-2-b
(let [steps 1000
      idx (range 1 (inc steps))]
  (doto (line-chart idx (actions steps :epsilon 0)
                    :series-label "epsilon: 0"
                    :legend true
                    :x-label "Step"
                    :y-label "Optimal action"
                    :title "Average performance of ε-greedy action-value methods on the 10-armed testbed")
    (add-categories idx (actions steps :epsilon 0.01) :series-label "epsilon: 0.01")
    (add-categories idx (actions steps :epsilon 0.1) :series-label "epsilon: 0.1")
    view))

;; figure-2-3
(let [steps 1000
      idx (range 1 (inc steps))]
  (doto (line-chart idx (actions steps :epsilon 0 :initial 5 :method :constant)
                    :series-label "epsilon: 0 initial 5"
                    :legend true
                    :x-label "Step"
                    :y-label "Optimal action")
    (add-categories idx (actions steps :epsilon 0.1 :initial 0 :method :constant)
                    :series-label "epsilon 0.1 initial 0")
    view))

;; fiture-2-4
(let [steps 1000
      idx (range 1 (inc steps))]
  (doto (line-chart idx (rewards steps :ucb-degree 2)
                    :series-label "UCB c=2"
                    :legend true
                    :x-label "Step"
                    :y-label "Average reward"
                    :title "Average performance of ε-greedy action-value methods on the 10-armed testbed")
    (add-categories idx (rewards steps :epsilon 0.1) :series-label "epsilon: 0.1")
    view))

(let [steps 1000
      idx (range 1 (inc steps))]
  (doto (line-chart idx (actions steps :method :gradient :base-line true :step-size 0.1 :true-reward 4)
                    :x-label "Step" :y-label "Optimal action" :legend true
                    :series-label "base-line:true step-size:0.1")
    (add-categories idx (actions steps :method :gradient :base-line true :step-size 0.4 :true-reward 4) :series-label "base-line:true step-size:0.4")
    (add-categories idx (actions steps :method :gradient :base-line false :step-size 0.1 :true-reward 4) :series-label "base-line:false step-size:0.1")
    (add-categories idx (actions steps :method :gradient :base-line false :step-size 0.4 :true-reward 4) :series-label "base-line:false step-size:0.4")
    view))

(let [epsilon-greedy #(/ (apply + (rewards 1000 :method :sample-avg :epsilon %)) 1000)
      gradient #(/ (apply + (rewards 1000 :method :gradient :base-line true :step-size %)) 1000)
      ucb #(/ (apply + (rewards 1000 :ucb-degree %)) 1000)
      greedy-initial #(/ (apply + (rewards 1000 :epsilon 0 :initial %)) 1000)
      [xs1 xs2 xs3 xs4] (map #(map (fn [r] (Math/pow 2 r)) %)
                             [(range -7 -1) (range -5 2) (range -4 3) (range -2 3)])]
  (doto (line-chart xs1 (map epsilon-greedy xs1) :legend true :series-label "epsilon-greedy")
    (add-categories xs2 (map gradient xs2) :series-label "gradient bandit")
    (add-categories xs3 (map ucb xs3) :series-label "UCB")
    (add-categories xs4 (map greedy-initial xs4) :series-label "greedy with optimistic initialization")
    view))
