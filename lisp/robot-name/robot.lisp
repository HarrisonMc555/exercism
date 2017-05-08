(in-package #:cl-user)
(defpackage #:robot
  (:use #:common-lisp)
  (:export #:build-robot #:robot-name #:reset-name))

(in-package #:robot)

;; Shared set of all used names
(defvar *used-names* ())

(defun get-random-name ()
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (digits "0123456789")
         (random-char (lambda () (char chars (random 26))))
         (random-digit (lambda () (char digits (random 10)))))
    (coerce (list (funcall random-char)
                  (funcall random-char)
                  (funcall random-digit)
                  (funcall random-digit)
                  (funcall random-digit)) 'string)))

(defun get-unused-name ()
  (let ((name (get-random-name)))
    (loop while (member name *used-names*)
       do (setq name (get-random-name))
       collect (progn (push name *used-names*)
                      name))))

;; Build a robot
(defclass robot ()
  ((robot-name
    :initform (get-unused-name))))
