(in-package #:cl-user)
(defpackage #:grains
  (:use #:cl)
  (:export :square :total))
(in-package #:grains)

(defun square (n)
  "The number of grains of rice on a given square"
  ;; 2^(n-1)
  (expt 2 (1- n)))

(defun total ()
  "The total number of grains on the chessboard"
  ;; sum from i = 1 to n of 2^(i-1) = 2^n - 1
  (1- (expt 2 64)))
