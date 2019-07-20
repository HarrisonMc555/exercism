(ns reverse-string
  (:require [clojure.string :as str]))

(defn reverse-string [s]
  (if (= s "")
    ""
    (let [first (get s 0)
          rest (subs s 1)]
      (str (reverse-string rest) first))))
