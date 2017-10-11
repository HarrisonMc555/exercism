(in-package #:cl-user)
(defpackage #:robot
  (:use #:common-lisp)
  (:export #:build-robot #:robot-name #:reset-name))

(in-package #:robot)

(defvar *used-names* ()
  "Shared set of all used names.")

(defun random-char (str)
  "Gets a random character out of a string."
  (char str (random (length str))))

(defun get-random-name ()
  "Get a random valid name (two capital letters, three digits)."
  (let ((capital-letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (digits "0123456789"))
    (coerce (list (random-char capital-letters)
                  (random-char capital-letters)
                  (random-char digits)
                  (random-char digits)
                  (random-char digits)) 'string)))

(defun get-unused-name ()
  "Get a valid but unused robot name."
  (loop
     for name = (get-random-name) then (get-random-name)
     while (member name *used-names*)
     finally (progn (push name *used-names*)
                    (return name))))

(defclass robot ()
  ((robot-name
    :initform (get-unused-name)
    :reader robot-name
    :documentation "Unique name"))
  (:documentation "Robot with unique name."))

(defun build-robot ()
  "Creates a new robot object."
  (make-instance 'robot))

(defun reset-name (robot-obj)
  "Creates a new and unique name for robot."
  (setf (slot-value robot-obj 'robot-name) (get-unused-name)))
