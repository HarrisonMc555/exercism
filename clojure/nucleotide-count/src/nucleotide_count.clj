(ns nucleotide-count
  (:require [clojure.string :as string]))

(defn- in-string? [char string]
  (some? (string/index-of string char)))

(defn count [nucleotide strand]
  (if (not (in-string? nucleotide "AGCT"))
    (throw (IllegalArgumentException. "Invalid nucleotide"))
    (->> strand
         (filter #(= nucleotide %1))
         (clojure.core/count))))

(defn nucleotide-counts [strand]
  
)
