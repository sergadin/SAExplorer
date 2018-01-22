;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Encoding: utf-8; -*-

(defpackage :saexplorer-test-asd
  (:use :common-lisp :asdf))

(in-package :saexplorer-test-asd)

(asdf:defsystem :saexplorer-test
  :description "Tests for SAExplorer"
  :author "serg@msu.ru"
  :depends-on ("saexplorer" "lift")
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:file "conference-names")
   (:file "jsonpath")))
