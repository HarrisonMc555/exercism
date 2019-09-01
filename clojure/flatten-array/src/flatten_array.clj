(ns flatten-array)

;; 
(defn flatten [arr]
  "Flatten an nested sequence, stripping nil elements"
  (cond
    (nil? arr) nil
    (seqable? arr) (let [a (seq arr)]
                     (when (some? a)
                       (filter some? (concat
                                      (flatten (first a))
                                      (flatten (rest a))))))
    :else (cons arr nil)))
