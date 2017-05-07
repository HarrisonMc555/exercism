(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl)
  (:export #:from))
(in-package #:gigasecond)

(defun from (year month day hour minute second)
  (let* ((gmt 0) ;; Avoid daylight savings issues
         (new-date (multiple-value-list
                    (decode-universal-time
                     ;; Add 10^9 seconds to the given date
                     (+ (expt 10 9)
                        (encode-universal-time
                         second
                         minute
                         hour
                         day
                         month
                         year
                         gmt))
                     gmt)))
         ;; Extract pieces of result
         (new-second (first new-date))
         (new-minute (second new-date))
         (new-hour (third new-date))
         (new-day (fourth new-date))
         (new-month (fifth new-date))
         (new-year (sixth new-date)))
    ;; Put pieces back together in the right order
    (list new-year new-month new-day new-hour new-minute new-second)))
