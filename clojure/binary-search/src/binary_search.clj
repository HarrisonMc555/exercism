(ns binary-search)

(defn- average-int [x y]
  (quot (+ x y) 2))

(defn- search-for-helper [elem v start end]
  (if (or (>= start (count v)) (< end start))
    (throw (Exception. "not found"))
    (let [middle-index (average-int start end)
          middle-elem (nth v middle-index)
          comp-result (compare elem middle-elem)]
      (cond
        (neg? comp-result) (search-for-helper elem v start (dec middle-index))
        (pos? comp-result) (search-for-helper elem v (inc middle-index) end)
        :else middle-index))))

(defn search-for [elem v]
  (search-for-helper elem v 0 (count v)))

(defn middle [v]
  (average-int 0 (count v)))
