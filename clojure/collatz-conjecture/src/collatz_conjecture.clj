(ns collatz-conjecture)

(defn- next-collatz [num]
  (if (= 0 (mod num 2))
    (quot num 2)
    (+ (* 3 num) 1)))

(defn collatz [num]
  (if (< num 1)
    (throw (IllegalArgumentException. "num is less than 1"))
    (->> num
         (iterate next-collatz)
         (take-while #(not= 1 %1))
         (count))))
