(ns nucleotide-count
  (:require [clojure.string :as string]))

(def ^:private empty-nucleotides {\A 0 \C 0 \T 0 \G 0})

(defn count [nucleotide strand]
  (if-not (contains? empty-nucleotides nucleotide)
    (throw (IllegalArgumentException. "Invalid nucleotide"))
    (->> strand
         (filter #(= nucleotide %1))
         (clojure.core/count))))

(defn nucleotide-counts [strand]
  (let [inc-or-throw #(if-not (contains? %1 %2)
                         (throw (Exception. "Invalid nucleotide"))
                         (update %1 %2 inc))]
    (reduce inc-or-throw empty-nucleotides strand)))
