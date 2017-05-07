(defpackage #:leap
  (:use #:common-lisp)
  (:export #:leap-year-p))
(in-package #:leap)

(defun leap-year-p (year)
  "True if year is a leap year"
  ;; If it's divisible by four...
  (and (= 0 (mod year 4))
       ;; ...but not divisible by 100...
       (or (not (= 0 (mod year 100)))
           ;; ...unless it's also divisble by 400.
           (= 0 (mod year 400)))))
