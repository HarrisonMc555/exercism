(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:input-type)
  (:export #:response-for))
(in-package #:bob)

(defun yell-p (input)
  "Yelling contains letters, all of which are upper case."
  (and (some #'upper-case-p input) (notany #'lower-case-p input)))

(defun question-p (input)
  "A question ends in a question mark."
  (eql #\? (char input (1- (length input)))))

(defun silence-p (input)
  "Silence is zero or more spaces."
  (string= "" (string-trim " " input)))

(defun response-for (input)
  "Create the appropriate response for input."
  (cond ((silence-p input) "Fine. Be that way!")
        ((yell-p input) "Whoa, chill out!")
        ((question-p input) "Sure.")
        (:else "Whatever.")))
