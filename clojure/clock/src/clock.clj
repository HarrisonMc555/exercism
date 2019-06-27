(ns clock)

(defrecord Clock [hours minutes])

(def ^:private ^:const MINUTES_IN_HOUR 60)
(def ^:private ^:const HOURS_IN_DAY 24)
(def ^:private ^:const MINUTES_IN_DAY (* MINUTES_IN_HOUR HOURS_IN_DAY))

(defn clock
  ([hours minutes]
   (let [total-minutes (+ (* hours MINUTES_IN_HOUR) minutes)
         total-minutes-wrapped (mod total-minutes MINUTES_IN_DAY)
         new-hours (quot total-minutes-wrapped MINUTES_IN_HOUR)
         new-minutes (mod total-minutes-wrapped MINUTES_IN_HOUR)]
     (Clock. new-hours new-minutes)))
  ([minutes]
   (clock 0 minutes)))

(defn- clock-total-minutes [clock]
  (+ (* MINUTES_IN_HOUR (:hours clock)) (:minutes clock)))

(defn add-time [clock minutes]
  (clock/clock (+ (clock-total-minutes clock) minutes)))

(defn clock->string [clock]
  (format "%02d:%02d" (:hours clock) (:minutes clock)))
