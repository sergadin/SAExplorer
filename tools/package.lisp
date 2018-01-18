(in-package :cl-user)

(defpackage :saexplorer.tools
  (:use :cl :cl-log)
  (:use :saexplorer.models)
  (:import-from :saexplorer #:get-json-item)
  (:export #:update-dblp-conferences))
