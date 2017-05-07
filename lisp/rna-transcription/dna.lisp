(in-package #:cl-user)
(defpackage #:dna
  (:use #:cl)
  (:export #:to-rna))
(in-package #:dna)

(defun convert-base (base)
  "Transform a DNA nucleotide to an RNA nucleotide"
  (case base
    (#\G #\C)
    (#\C #\G)
    (#\T #\A)
    (#\A #\U)
    (otherwise (throw :dna-error "Invalid DNA nucleotide"))))

(defun to-rna (str)
  "Transcribe a string representing DNA nucleotides to RNA."
  (map 'string #'convert-base str))
