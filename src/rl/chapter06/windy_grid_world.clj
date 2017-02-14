;; gorilla-repl.fileformat = 1

;; @@
(ns rl.chapter06.windy-grid-world
  (:require [incanter.stats :refer [sample-binomial]]
            [clojure.core.matrix :as m]
            [rl.util :refer [argmax take-until]])
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
(def nrow 7)
(def ncol 10)
(def actions [[-1 0] ;; up
              [1 0]  ;; down
              [0 -1] ;; left
              [0 1]  ;; right
])
(def actions-idx (into {} (map-indexed #(vector %2 %1) actions)))

(def init-state-action-values (m/zero-array [nrow ncol (count actions)]))

(def start-state [3 0])
(def dest-state [3 7])
(def wind [0 0 0 1 1 1 2 2 1 0])

(defn one-episode
  [[_ state-action-values]]
  (let [epsilon 0.1
        alpha 0.5
        reward -1
        get-action (fn [sav [r c]]
                     (if (zero? (sample-binomial 1 :prob 0.1))
                       (->> (ffirst (argmax (m/select sav r c :all)))
                            int
                            (nth actions))
                       (rand-nth actions)))
        sarsa-update (fn [sav [cur-r cur-c] cur-a [next-r next-c] next-a]
                       (let [cur-a-idx (actions-idx cur-a)
                             next-a-idx (actions-idx next-a)]
                         (update-in sav [cur-r cur-c cur-a-idx]
                                    #(+ %
                                        (* alpha (+ reward
                                                    (get-in sav [next-r next-c next-a-idx])
                                                    (- (get-in sav [cur-r cur-c cur-a-idx]))))))))
        update-state (fn [[row col] [row-a col-a]]
                       (let [w (- (wind col))
                             row-new (-> (+ row row-a w)
                                         (max 0)
                                         (min (dec nrow)))
                             col-new (-> (+ col col-a)
                                         (max 0)
                                         (min (dec ncol)))]
                         [row-new col-new]))
        run (fn [[state action sav]]
              (let [new-state (update-state state action)
                    new-action (get-action sav new-state)
                    new-sav (sarsa-update sav state action new-state new-action)]
                [new-state new-action new-sav]))
        end? (fn [[state action sav]] (= state dest-state))
        res (take-until end? (iterate run [start-state
                                           (get-action state-action-values start-state)
                                           state-action-values]))]
    [(count res) (last (last res))]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter06.windy-grid-world/one-episode</span>","value":"#'rl.chapter06.windy-grid-world/one-episode"}
;; <=

;; @@
(def result-200 (take 200 (iterate one-episode [0 init-state-action-values])))
(def result-2000 (take 2000 (iterate one-episode [0 init-state-action-values])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rl.chapter06.windy-grid-world/result-2000</span>","value":"#'rl.chapter06.windy-grid-world/result-2000"}
;; <=

;; @@
(-> (plotly)
    (add-scatter :y (apply concat (map-indexed #(repeat (first %2) %1)
                                               (rest result-200))))
    (plot "RL-figure-6-4" :fileopt "overwrite")
    embed-url)
;; @@
;; =>
;;; {"type":"html","content":"<iframe height=\"600\" src=\"//plot.ly/~findmyway/140.embed\" width=\"800\"></iframe>","value":"pr'ed value"}
;; <=

;; @@
(m/pm (m/reshape (map #({0 :up 1 :down 2 :left 3 :right} 
                           (ffirst (argmax %))) 
                      (m/reshape (last (last result-2000))
                                 [70 4]))
                 [7 10]))
;; @@
;; ->
;;; [[ :down  :down :right :right :right :right :right :right :right :down]
;;;  [:right :right :right :right    :up :right  :down    :up    :up :down]
;;;  [:right    :up :right  :left :right :right :right  :down    :up :down]
;;;  [:right :right :right :right :right :right :right    :up :right :down]
;;;  [ :down :right :right :right :right :right    :up  :down  :left :left]
;;;  [ :down :right :right  :left :right    :up    :up    :up  :left :down]
;;;  [:right :right :right :right    :up    :up    :up    :up    :up :left]]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
