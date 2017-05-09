(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl)
  (:export #:from))
(in-package #:gigasecond)

(defun from-standardized (second minute hour day month year)
  (let* ((gigasecond (expt 10 9))
         (gmt 0)
         (cur-seconds (encode-universal-time second minute hour day month year
                                             gmt)))
    (decode-universal-time (+ gigasecond cur-seconds) gmt)))

(defun from (year month day hour minute second)
  (multiple-value-bind (second minute hour day month year)
      (from-standardized second minute hour day month year)
    (list year month day hour minute second)))

