(in-package :cl-user)

(defpackage :saexplorer.terminology
  (:nicknames :terms)
  (:use :cl)
  (:import-from :cl-log
                #:log-message)
  (:export #:extract))
