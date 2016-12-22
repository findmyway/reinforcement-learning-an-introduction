(ns rl.chapter04.car-rental
  (:require [incanter.stats :refer [pdf-poisson]]
            [clojure.core.matrix :as m]
            [rl.util :refer [argmax]])
  (:use debux.core))

(def max-cars 20)
(def max-move 5)
(def lambda-rent-1 3)
(def lambda-rent-2 4)
(def lambda-return-1 3)
(def lambda-return-2 2)
(def discount 0.9)
(def rental 10)
(def move-cost 2)

(def poisson-bound 12) ;; (cdf-poisson 11 :lambda 4) => 0.99908477085273)
(def actions (range (- max-move) (inc max-move)))

(def prob-rent1 (memoize  #(pdf-poisson % :lambda lambda-rent-1)))
(def prob-rent2 (memoize #(pdf-poisson % :lambda lambda-rent-2)))
(def prob-return1 (memoize #(pdf-poisson % :lambda lambda-return-1)))
(def prob-return2 (memoize #(pdf-poisson % :lambda lambda-return-2)))

(def possible-req (for [i (range poisson-bound) j (range poisson-bound)] [i j]))
(def possible-ret possible-req)
(def states (for [i (range (inc max-cars)) j (range (inc max-cars))] [i j]))
(def init-policy (m/zero-matrix (inc max-cars) (inc max-cars)))
(def init-statevalue (m/zero-matrix (inc max-cars) (inc max-cars)))

(defn expected-return
  [[s1 s2] action state-value]
  (let [ac-cost (* (- move-cost) (Math/abs action))
        n-cars-p1 (min (- s1 action) max-cars)
        n-cars-p2 (min (+ s2 action) max-cars)
        eval-return (fn [sv r p [ret1 ret2] [rem1 rem2]]
                      (let [rem1 (min max-cars (+ rem1 ret1))
                            rem2 (min max-cars (+ rem2 ret2))
                            prob (* p (prob-return1 ret1) (prob-return2 ret2))]
                        (* prob (+ r (* discount (get-in sv [rem1 rem2]))))))
        get-r (fn [sv [req1 req2] [rem1 rem2]]
                (let [real-req1 (min req1 rem1)
                      real-req2 (min req2 rem2)
                      reward (* rental (+ real-req1 real-req2))
                      left1 (- rem1 real-req1)
                      left2 (- rem2 real-req2)
                      prob (* (prob-rent1 req1) (prob-rent2 req2))]
                  (reduce + (map #(eval-return sv reward prob % [left1 left2]) possible-ret))))]
    (reduce + ac-cost (map #(get-r state-value % [n-cars-p1 n-cars-p2]) possible-req))))

(defn update-sv
  [sv policy]
  (m/reshape
   (map #(expected-return % (int (get-in policy %)) sv) states)
   (m/shape sv)))

(defn converge?
  [[a b]]
  (let [diff (m/abs (- (m/esum a) (m/esum b)))
        record-file "resources/data/car-rental-statavalue-converge"]
    (do
      (spit record-file (str diff "\n")  :append true)
      (< diff 1))))

(defn find-converge
  [sv policy]
  (let [helper #(update-sv % policy)]
    (ffirst (filter converge? (partition 2 (iterate helper sv))))))

(defn update-pl
  [state-value policy]
  (let [new-pl (fn [[i j]]
                 (let [ac (filter #(and (<= % i) (>= % (- j))) actions)
                       ac-returns (map #(expected-return [i j] % state-value) ac)]
                   (nth ac (ffirst (argmax ac-returns)))))]
    (m/reshape (map new-pl states) (m/shape policy))))

;; It is really slow... (≈30minutes on my Mac)
(def res
  (loop [old-sv init-statevalue
         old-pl init-policy]
    (let [sv (find-converge old-sv old-pl)
          pl (update-pl sv old-pl)]
      (if (not= pl  old-pl)
        (do
          (spit "resources/data/car-rental-policy" (str pl "\n") :append true)
          (recur sv pl))
        [sv pl]))))

(spit "resources/data/car-rental-statevalue" (first res))
