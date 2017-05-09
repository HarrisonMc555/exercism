(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl)
  (:export #:from))
(in-package #:gigasecond)

(defun encode-universal-time* (time-values)
  (apply #'encode-universal-time (reverse time-values)))

(defun decode-universal-time* (universal-time tz)
  (reverse (subseq (multiple-value-list
                    (decode-universal-time universal-time tz)) 0 6)))

(defun from (year month day hour minute second)
  "Returns date 1 gigasecond from given date"
  ;; Use gmt time zone to avoid time zone pitfalls
  (let* ((gmt 0)
         (time-values (list gmt year month day hour minute second)))
     (decode-universal-time*
      (+ (expt 10 9) (encode-universal-time* time-values))
      gmt)))
