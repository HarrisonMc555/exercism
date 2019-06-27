(ns etl)

(defn- add-all-to-map [mapping value keys]
  (reduce #(assoc %1 %2 value) mapping (map clojure.string/lower-case keys)))

(defn transform [points-to-letters]
  (reduce-kv #(add-all-to-map %1 %2 %3) {} points-to-letters))
