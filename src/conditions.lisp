;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(in-package :saexplorer.sys)


(define-condition generic-error (simple-error)
  ((text :initarg :text :reader text))
  (:documentation "Represents general parsing error."))

(defun generic-error (format &rest args)
  (error 'generic-error
         :format-control format
         :format-arguments args))
