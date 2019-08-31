(ns reverse-string)

(defn- my-rev [vector]
  (let [indices (range (dec (count vector)) -1 -1)]
    (mapv #(nth vector %) indices)))

(defn reverse-string [s]
  (let [forward-vec (char-array s)
        reverse-vec (my-rev forward-vec)]
    (apply str reverse-vec)))

(defn- rev-accum [forward reversed]
  (let [x (first forward)]
    (if (nil? x)
      reversed
      (rev-accum (rest forward) (cons x reversed)))))

(defn- my-rev2 [sq]
  (apply str (rev-accum sq ())))

(defn- reverse-string2 [s]
  (apply str (my-rev2 s)))
