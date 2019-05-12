(ns armstrong-numbers
  (:require [clojure.math.numeric-tower :as math]))

(defn digits
  ([num] (digits num 10))
  ([num base] (->> num
              (iterate #(quot % base))
              (take-while pos?)
              (mapv #(mod % base))
              (rseq))))

(defn armstrong? [x]
  (let [x-digits (vec (digits x))
        num-of-digits (count x-digits)
        powered-digits (map #(math/expt %1 num-of-digits) x-digits)
        sum (reduce + powered-digits)]
    (= x sum)))
