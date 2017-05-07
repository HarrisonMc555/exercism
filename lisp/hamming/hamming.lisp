(defpackage #:hamming
  (:use #:cl)
  (:export #:distance))

(in-package #:hamming)

(defun distance (dna1 dna2)
  "Number of positional differences in two equal length dna strands."
  ;; If not the same length, then nil
  (if (not (= (length dna1) (length dna2)))
      nil
      ;; If same length, see how many characters are unequal
      (length (remove t (mapcar 'eql
                                ;; Get a list of characters
                                (map 'list #'identity dna1)
                                (map 'list #'identity dna2))))))
