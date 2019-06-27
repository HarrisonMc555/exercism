(ns complex-numbers
  (:require [clojure.math.numeric-tower :as math]))

(defn real [[a b]]
  a)

(defn imaginary [[a b]]
  b)

(defn abs [[a b]]
  (float (math/sqrt (+ (* a a) (* b b)))))

(defn conjugate [[a b]]
  [a (- b)])

(defn add [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn sub [[a b] [c d]]
  [(- a c) (- b d)])

(defn mul [[a b] [c d]]
  [(- (* a c) (* b d)) (+ (* a d) (* b c))])

(defn div [num1 num2]
  (let [[new-a new-b] (mul num1 (conjugate num2))
        [c d] num2
        num2-abs-squared (+ (* c c) (* d d))]
    [(float (/ new-a num2-abs-squared))
     (float (/ new-b num2-abs-squared))]))
    ;; [(/ new-a num2-abs-squared)
    ;;   (/ new-b num2-abs-squared)]))
