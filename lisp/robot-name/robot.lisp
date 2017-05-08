(in-package #:cl-user)
(defpackage #:robot
  (:use #:common-lisp)
  (:export #:build-robot #:robot-name #:reset-name))

(in-package #:robot)

;; Shared set of all used names
(defvar *used-names* ())

;; Get a random valid name (two capital letters, three digits)
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

;; Get a valid unused name
(defun get-unused-name ()
  ;; Create random names until you find one that hasn't been used yet
  (let ((name (get-random-name)))
    (loop while (member name *used-names*)
       do (setq name (get-random-name)))
    (push name *used-names*)
    name))

;; Robot class
(defclass robot ()
  ;; Initialize name to an unused name
  ((robot-name
    :initform (get-unused-name)
    :reader robot-name
    :documentation "Robot's name")))

;; Build a robot
(defun build-robot ()
  (make-instance 'robot))

;; Reset a robot's name
(defun reset-name (robot-obj)
  (setf (slot-value robot-obj 'robot-name) (get-unused-name)))
