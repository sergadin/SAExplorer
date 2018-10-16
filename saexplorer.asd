;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Encoding: utf-8; -*-

(in-package :cl-user)

(defparameter *project-home*
  (pathname (directory-namestring *load-truename*))
  "Home directory.")

(defpackage #:saexplorer-asd
  (:use :common-lisp :asdf))

(in-package :saexplorer-asd)

(pushnew :debug *features*)

#+(and :ccl :debug)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim
   (optimize
    (safety 3)	                ; Error checking
    (speed 0)	                ; Speed of the compiled code
    (compilation-speed 0)       ; Speed of compilation
    (space 1)                   ; Space of both intermediate files and object
    (debug 3))))


;;; Define some 'production' compilation parameters

#+:production
(proclaim '(optimize
            (safety 0)		        ; Run time error checking level
            (speed 3)			; Speed of the compiled source
            (compilation-speed 0)	; Speed of compilation
            (space 0)			; Space of both intermediate files and object
            (debug 0)))


(asdf:defsystem :saexplorer
  :name "Scientific areas explorer"
  :description ""
  :version "0.2"
  :author "serg@msu.ru"
  :depends-on ("cl-log"
               "plump" "clss" ; HTML parsers
               "cl-containers"
               "cl-utilities" "alexandria" "rutils" "cl-singleton-mixin"
               "optima" ; pattern matching
               "drakma" "dexador" "do-urlencode" "html-entities"
               "cl-json" "cxml" "cxml-stp" "xuriella" ; data formats
               "cl-dbi" "dbd-sqlite3" "md5" "mito" ; database access
               "bordeaux-threads" "usocket" "hunchentoot" "clack" "quri" ; web server
               "hunchensocket" "websocket-driver"; websockets
               "fare-memoization" "cl-redis" ; caching
               "rss" ; RSS parser
               "esrap"
               ; "closure-html"
               "py-configparser" "unix-opts")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "specials")
               (:file "conditions")
               ;; useful tools
               (:module utils
                        :components
                        ((:file "package")
                         (:file "logging")
                         (:file "parsing")
                         (:module jsonpath
                                  :components
                                  ((:file "parser")
                                   (:file "jsonpath")))
                         (:file "cache")
                         (:file "fetch")
                         (:file "rss")
                         (:file "sequences")
                         (:file "keywords")))
               (:module models
                        :components
                        ((:file "package")
                         (:file "constants")
                         (:file "common")
                         (:file "conf")))
               (:file "bibsystem")
               (:file "documents")
               (:file "query")
               (:module systems
                        :components
                        ((:file "scopus")
                         (:file "wos")
                         (:file "crossref")
                         (:file "springer")
                         (:file "aminer")
                         (:file "dblp")))
               (:module extraction
                        :components
                        ((:file "package")
                         (:file "dates")))
               (:module cfp
                        :components
                        ((:file "package")
                         (:file "structure")
                         (:file "collect")
                         (:module spiders
                                  :components
                                  ((:file "omicsonline.org")
                                   (:file "drugs.com")
                                   (:file "elsevier")
                                   (:file "wikicfp")))))
               (:file "keywords")
               (:file "conferences")
               (:file "explorer")
               (:file "webserver") ; web server starter and tools
               (:file "wsapp")
               (:module ws-resources
                        :components
                        ((:file "package")
                         (:file "conference")))
               ;; top-level function that starts everything
               (:file "main"))
  :in-order-to ((test-op (load-op saexplorer-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (symbol-name '#:run-tests) :lift)
                             :config :generic)))

(defmethod operation-done-p
           ((o test-op) (c (eql (find-system 'saexplorer))))
  (values nil))
