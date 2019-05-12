(ns hamming)

(defn distance [strand1 strand2]
  (if (= (count strand1) (count strand2))
    (let [pairs (map vector strand1 strand2)
          different-pairs (filter #(apply not= %1) pairs)
          num-different-pairs (count different-pairs)]
      num-different-pairs)))
