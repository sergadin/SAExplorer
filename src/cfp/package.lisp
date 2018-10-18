(in-package :cl-user)

(defpackage :saexplorer.cfp
  (:nicknames :cfp)
  (:use :cl)
  (:import-from :cl-log
                #:log-message)
  (:import-from :saexplorer.models
                #:<cfp-page>
                #:cfp-name #:cfp-source)
  (:export #:collect
           #:<cfp-info>))
