(ns armstrong-numbers
  (:require [clojure.math.numeric-tower :as math]))

;; Stolen from https://stackoverflow.com/a/29942388/7343786.
;; I understand what it's doing and how now. The only thing I'm not sure I fully
;; understand is why they're using `mapv` and `resq` instead of `map` and
;; `reverse`. I think it's because it will all have to be evaluated anyways, so
;; trying to be lazy doesn't help much.
(defn- digits
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
