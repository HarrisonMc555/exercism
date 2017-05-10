(defpackage #:leap
  (:use #:common-lisp)
  (:export #:leap-year-p))
(in-package #:leap)

(defun divisble-by (num divisor)
  "True if num is divisible by divisor."
  (zerop (mod num divisor)))

(defun leap-year-p (year)
  "True if year is a leap year."
  (and (divisble-by year 4)
       (or (not (divisble-by year 100))
           (divisble-by year 400))))
