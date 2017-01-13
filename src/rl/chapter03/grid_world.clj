;; gorilla-repl.fileformat = 1

;; @@
(ns rl.chapter03.grid-world
  (:require [clojure.core.matrix :as m]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
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
  (m/pm (converge update-w1))
  (m/pm (converge update-w2)))

;; @@
;; ->
;;; [[ 3.309  8.789  4.428  5.322  1.492]
;;;  [ 1.522  2.992  2.250  1.908  0.547]
;;;  [ 0.051  0.738  0.673  0.358 -0.403]
;;;  [-0.974 -0.435 -0.355 -0.586 -1.183]
;;;  [-1.858 -1.345 -1.229 -1.423 -1.975]]
;;; [[21.977 24.419 21.977 19.419 17.477]
;;;  [19.780 21.977 19.780 17.802 16.022]
;;;  [17.802 19.780 17.802 16.022 14.419]
;;;  [16.022 17.802 16.022 14.419 12.977]
;;;  [14.419 16.022 14.419 12.977 11.680]]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
