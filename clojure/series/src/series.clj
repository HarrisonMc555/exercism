(ns series)

(defn slices [string length]
  (if (= 0 length)
    [""]
    (->> (range)
         (map #(vector %1 (+ %1 length)))
         (take-while #(<= (get %1 1) (count string)))
         (map #(apply subs string %1)))))
