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
