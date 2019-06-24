(ns clock)

(defrecord Clock [hours minutes])

(defn clock->string [clock]
  (format "%02d:%02d" (:hours clock) (:minutes clock)))

(defn clock [hours minutes]
  (Clock. hours minutes))

(defn add-time [clock minutes]
  (update clock :minutes + minutes))
