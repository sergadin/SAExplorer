;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(in-package :saexplorer.sys)


(define-condition generic-error (simple-error)
  ((text :initarg :text :reader text))
  (:documentation "Most general SAExplorer condition."))


(define-condition invalid-request-error (generic-error)
  ()
  (:documentation "Signaled when a user submitted an invalid request ."))


(define-condition not-implemented-error (generic-error)
  ()
  (:documentation "Signaled when a user requests unimplemented operation."))

(defun saexplorer-error (condition format &rest args)
  (error condition
         :format-control format
         :format-arguments args))
