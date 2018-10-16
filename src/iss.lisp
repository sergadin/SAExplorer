;;;;
;;;; Internet search engine
;;;;

(defpackage :saexplorer.internet-search-engine
  (:nicknames :iss)
  (:use :cl)
  (:import-from :cl-log
                #:log-message)
  (:export #:impact
           #:find-relevant
           #:similar))

(defclass <internet-resource> (<document>)
  ((url :type string :accessor resource-url)
   (title :type string :accessor resource-title)
   (mime-type)
   (charset)
   (language)
   (modification-time))
