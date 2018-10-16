(in-package :cl-user)

(defpackage saexplorer.utils
  (:nicknames :sa-utils)
  (:use :cl :saexplorer.sys)
  (:import-from :cl-log
                #:log-message)
  (:export #:fetch-url))
