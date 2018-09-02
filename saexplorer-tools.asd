(in-package :cl-user)

(defpackage :saexplorer-tools-asd
  (:use :common-lisp :asdf))

(in-package :saexplorer-tools-asd)

(asdf:defsystem :saexplorer-tools
  :description "Collection of tools for SAExplorer"
  :author "serg@msu.ru"
  :depends-on ("saexplorer"
               "cl-json" "cl-csv" "mito"
               "local-time" "local-time-duration"
               "lparallel" "bt-semaphore")
  :pathname "tools"
  :serial t
  :components
  ((:file "package")
   (:module mag
            :components
            ((:file "in-memory-db")
             (:file "elastic-search")
             (:file "process")
             (:file "indexer")))
   (:file "core-confs")
   (:file "dblp-conferences")))
