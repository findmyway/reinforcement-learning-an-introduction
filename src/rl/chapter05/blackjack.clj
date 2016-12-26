(ns rl.chapter05.blackjack
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as stats]
            [rl.util :refer [argmax find-index]])
  (:use [debux.core]))

(defn bust? [x] (> x 21))
(def not-bust? (complement bust?))

(defn get-card
  "Generate a card randomly. Face cards are replaced with 10"
  []
  (rand-nth (map #(min % 10) (range 1 14))))

(def actions [:hit :stand])

(defn init-dealer
  "Return: [card-value is-usableA]
  The dealer will get a random card. If the card is A,
  we replace it with 11 and set the is-usableA to be true."
  []
  (let [c (get-card)]
    (if (= c 1)
      [11 true]
      [c false])))

(defn init-player
  "Return: [sum is-usableA]
  Conitnue to hit until player get a sum over 11.
  If sum is more than 21, the player must have one or two 11(A).
  Since sum should be between [12 21], one 11(A) is unusable.
  Then we have [(- s 10) (pos? (dec usableA))] returned."
  []
  (loop [s 0 usableA 0]
    (cond
      (> s 21) [(- s 10) (pos? (dec usableA))]
      (> s 11) [s (pos? usableA)]
      :else (let [c (get-card)]
              (if (= c 1)
                (recur (+ s 11) (inc usableA))
                (recur (+ s c) usableA))))))

(defn player-policy
  "The player's basic policy.
  We ignore the usableA flag here."
  [s is-usableA]
  (cond
    (< s 20) :hit
    :else :stand))

(defn dealer-policy
  "The dealer's basic policy."
  [s is-usableA]
  (cond
    (< s 17) :hit
    :else :stand))

(defn rand-policy
  [s is-usableA]
  (rand-nth actions))

(defn apply-policy
  "Return: [sum is-usableA traces]
  If bust, the sum will be nil.
  Traces is used to record the [sum is-usableA action] in every step.
  "
  [init-s is-usableA policy traces & {:keys [action]}]
  (let [action (if (nil? action) (policy init-s is-usableA) action)]
    (case action
      :stand [init-s is-usableA traces]
      :hit (let [c (get-card)]
             (if (bust? (+ init-s c))
               (if is-usableA
                 (let [s (+ init-s c -10)
                       a (policy s false)]
                   (apply-policy s false policy (conj traces [s false a])))
                 [nil is-usableA traces])
               (let [s (+ init-s c)
                     a (policy s is-usableA)]
                 (apply-policy s is-usableA policy (conj traces [s is-usableA a]))))))))

(defn play-with-init
  "Initial player and dealer's state info"
  [player-info dealer-info]
  (let [[p-res p-usableA p-traces] (apply apply-policy player-info)
        [d-res d-usableA d-traces] (apply apply-policy dealer-info)]
    (cond
      (nil? p-res) [-1 p-traces d-traces]
      (nil? d-res) [1  p-traces d-traces]
      :else [(compare p-res d-res) p-traces d-traces])))

(defn play
  []
  (let [[p-s p-usableA :as player-info] (init-player)
        p-a (player-policy p-s p-usableA)
        [d-s d-usableA :as dealer-info] (init-dealer)
        d-a (dealer-policy d-s d-usableA)
        d-s (if (= 11 d-s) 1 d-s) ;; turn back to original card number
        state [p-usableA p-s d-s]]
    [state
     (play-with-init (conj player-info player-policy [[p-s p-usableA p-a]])
                     (conj dealer-info dealer-policy [[d-s d-usableA d-a]]))]))

(defmacro update-m!
  "Update the mutable matrix in idx using f.
  f accept old value and update the matrix with returned value"
  [m f & idx]
  `(let [old# (m/mget ~m ~@idx)]
     (m/mset! ~m ~@idx (~f old#))))

(defn data-for-fig-5-1
  [n]
  (let [results (for [i (range n)] (play))

        states-usableA (filter #(ffirst %) results)
        states-usableA-counts (m/mutable (m/add (m/zero-matrix 10 10) 1))
        states-usableA-rewards (m/mutable (m/zero-matrix 10 10))
        _ (doall (for [[[_ s-p s-d] [r _ _]] states-usableA]
                   (let [i (- s-p 12) j (dec s-d)]
                     (do
                       (update-m! states-usableA-counts inc i j)
                       (update-m! states-usableA-rewards #(+ r %) i j)))))

        states-nousableA (filter #(not (ffirst %)) results)
        states-nousableA-counts (m/mutable (m/add (m/zero-matrix 10 10) 1))
        states-nousableA-rewards (m/mutable (m/zero-matrix 10 10))
        _ (doall (for [[[_ s-p s-d] [r _ _]] states-nousableA]
                   (let [i (- s-p 12) j (dec s-d)]
                     (do
                       (update-m! states-nousableA-counts inc i j)
                       (update-m! states-nousableA-rewards #(+ r %) i j)))))]
    [(m/div states-usableA-rewards states-usableA-counts)
     (m/div states-nousableA-rewards states-nousableA-counts)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def state-action-value (m/mutable (m/zero-array [10 10 2 2])))
(def state-action-count (m/mutable (m/add (m/zero-array [10 10 2 2]) 1)))

(defn behavior-policy
  [p-s p-A d-card state-action-value]
  (let [i (- p-s 12) j (dec d-card) k (if (true? p-A) 1 0)]
    (->> (m/select state-action-value i j k :all)
         argmax
         ffirst
         (nth actions))))

(defn mc-est
  "Play the blackjack with behavior-policy.
  We need the state-action-value to determin player's action.
  Notice that the initial action is generated randomly
  (ensure all possible initial states are covered)."
  [state-action-value]
  (let [[p-state p-usableA] (init-player)
        [d-state d-usableA] (init-dealer)
        d-card (if (= d-state 11) 1 d-state)  ;; turn back to original card number
        p-action (rand-nth actions)
        p-policy #(behavior-policy %1 %2 d-card state-action-value)
        player-info [p-state p-usableA p-policy [[p-state p-usableA p-action]] :action p-action]
        dealer-info [d-state d-usableA dealer-policy [[d-state d-usableA (dealer-policy d-state d-usableA)]]]]
    (play-with-init player-info dealer-info)))

(defn update!
  "play once using the mc-est"
  []
  (let [[r p-traces d-traces] (mc-est state-action-value)
        d-s (ffirst d-traces)
        d-card (if (= d-s 11) 1 d-s)  ;; turn back to original card number
        update-trace! (fn [[p-s p-usableA p-action]]
                        (let [i (- p-s 12)
                              j (dec d-card)
                              k (if (true? p-usableA) 1 0)
                              x (find-index actions p-action)]
                          (update-m! state-action-value #(+ r %) i j k x)
                          (update-m! state-action-count inc i j k x)))]
    (dorun (map update-trace! p-traces))))

;; play 500000 times and update state-action-value and state-action-count
(dorun (repeatedly 500000 update!))

;; The result is a little different from the figure-5-3
;; I'm not sure  where the problem is.
(let [sav (m/div state-action-value state-action-count)
      idx (for [i (range 10) j (range 10)] [i j])]
  [(map (fn [[i j]] (ffirst (argmax (m/select sav i j 0 :all)))) idx)
   (map (fn [[i j]] (ffirst (argmax (m/select sav i j 1 :all)))) idx)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-ratio-reward
  []
  (let [dealer-info [2 false dealer-policy [[2 false :hit]]]
        rand-action (rand-policy 13 true)
        player-info [13 true rand-policy [[13 true rand-action]] :action rand-action]
        [r p-traces d-traces] (play-with-init player-info dealer-info)
        real-actions (map #(nth % 2) p-traces)
        policy-actions (map (fn [[s is-A _]] (player-policy s is-A)) p-traces)
        importance-ratio (if (= real-actions policy-actions)
                           (Math/pow 2.0 (count real-actions))
                           0)]
    [importance-ratio r]))

(defn get-sampling
  [n]
  (let [ratios-rewards (repeatedly n get-ratio-reward)
        ratios-sum (reductions + (map first ratios-rewards))
        rewards-sum (reductions + (map #(apply * %) ratios-rewards))
        ordinary-sampling (m/div (m/matrix rewards-sum) (m/matrix (range 1 (inc n))))
        weighted-sampling (map #(if (zero? %2) 0 (/ %1 %2)) rewards-sum ratios-sum)]
    [ordinary-sampling weighted-sampling]))

(defn off-policy
  "Set n = 100 to get data for figure-5-4."
  [n]
  (let [true-value -0.27726
        n-episodes 10000
        samplings (for [_ (range n)] (get-sampling n-episodes))
        mean-square-shift (fn [s]
                            (->> (m/add s (- true-value))
                                 m/square
                                 m/transpose
                                 (map #(stats/mean %))))
        ordinary-sampling (mean-square-shift (map first samplings))
        weighted-sampling (mean-square-shift (map second samplings))]
    [ordinary-sampling weighted-sampling]))
