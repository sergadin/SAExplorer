(in-package :cl-user)

(defpackage :saexplorer-tools-asd
  (:use :common-lisp :asdf))

(in-package :saexplorer-tools-asd)

(asdf:defsystem :saexplorer-tools
  :description "Collection of tools for SAExplorer"
  :author "serg@msu.ru"
  :depends-on ("saexplorer" "cl-json" "mito")
  :pathname "tools"
  :serial t
  :components
  ((:file "package")
   (:file "dblp-conferences")))
