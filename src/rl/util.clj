(ns rl.util)

(defn argmax
  "Return the [index-value-pair...]. All occurrences are returned."
  [xs & {:keys [keep-order] :or {keep-order true}}]
  (when-let [xs-indexed (seq (map-indexed vector xs))]
    (loop [res [(first xs-indexed)]
           max-val (first xs)
           xs-indexed (rest xs-indexed)]
      (if (seq xs-indexed)
        (let [pair (first xs-indexed)
              comp-res (compare (second pair) max-val)]
          (case comp-res
            0 (recur (conj res pair) max-val (rest xs-indexed))
            1 (recur [pair] (second pair) (rest xs-indexed))
            -1 (recur res max-val (rest xs-indexed))))
        res))))

(defn find-index
  "Find the index of x in xs. 
  Return nil if not found"
  [xs x]
  (ffirst (filter #(= (second %) x) (map-indexed vector xs))))

(defn rand-weighted
  "Return a random key according to the weight value."
  [m]
  (let [xs (keys m)
        ws (vals m)
        v (rand (apply + ws))]
    (loop [ws (vec (reductions + 0 ws))
           left 0
           right (count m)]
      (if (= 1 (- right left))
        (nth xs left)
        (let [mid (quot (+ right left) 2)]
          (cond
            (< v (nth ws mid)) (recur ws left mid)
            (>= v (nth ws mid)) (recur ws mid right)))))))

(defn take-until
  "Take all elements until (pred item) return true.
  That element is included in the return coll"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first coll))
       [(first coll)]
       (cons (first s) (take-until pred (rest s)))))))
