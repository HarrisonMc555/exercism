(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:input-type)
  (:export #:response-for))
(in-package #:bob)

;; Create the appropriate response type for input
(defun response-type (input)
  (let ((trimmed-input (string-trim " " input)))
    (cond
      ;; Is it all spaces or empty string?
      ((string= "" trimmed-input)
       :silence)
      ;; Are all the letters uppercase (and actually contain letters)?
      ((and
        ;; Contains letters
        (some #'alpha-char-p trimmed-input)
        ;; All upper case
        (string= input (string-upcase trimmed-input)))
       :yell)
      ;; Is the last character a question mark?
      ((eql #\? (char trimmed-input (1- (length trimmed-input))))
       :question)
      ;; Default
      (t :default))))

;; Create the appropriate response given the response type
(defun response-for (input)
  (case (response-type input)
    (:yell "Whoa, chill out!")
    (:question "Sure.")
    (:silence "Fine. Be that way!")
    (otherwise "Whatever.")))
