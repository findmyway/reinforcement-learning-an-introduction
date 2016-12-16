(ns rl.chapter03.grid-world
  (:require [clojure.core.matrix :as m])
  (:use debux.core))

(let [N 5
      discount 0.9
      world (m/zero-matrix N N)
      mesh-idx (for [i (range N) j (range N)] [i j])
      actions [[-1 0] [1 0] [0 -1] [0 1]]  ;; [:left :right :down :up]
      prob (zipmap actions (repeat 0.25))
      get-next-states (fn [idx action]
                        (case idx
                          [0 1] [4 1]
                          [0 3] [2 3]
                          (mapv + idx action)))
      get-rewards (fn [idx action]
                    (cond
                      (= [0 1] idx) 10
                      (= [0 3] idx) 5
                      (some #{(mapv + idx action)} mesh-idx) 0
                      :else -1))
      update-w1 (fn [w idx]
                  (apply + (map #(* (prob %)
                                    (+ (get-rewards idx %)
                                       (* discount (get-in w (get-next-states idx %) (get-in w idx)))))
                                actions)))
      update-w2 (fn [w idx]
                  (apply max (map #(+ (get-rewards idx %)
                                      (* discount (get-in w (get-next-states idx %) (get-in w idx))))
                                  actions)))
      iterate-fn (fn [update-f w]
                   (->> (map (partial update-f w) mesh-idx)
                        (partition (count w))
                        (mapv vec)))
      stop? (fn [[a b]] (< (m/abs (- (m/esum a) (m/esum b))) 0.0001))
      converge #(ffirst (filter stop? (partition 2 (iterate (partial iterate-fn %) world))))]
  (prn (converge update-w1))
  (prn (converge update-w2)))
