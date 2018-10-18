
(in-package :cl-user)

(defpackage saexplorer.models
  (:nicknames :models)
  (:use :cl)
  (:export #:<bibdb> #:<entity>
           #:<conf-type> #:<conf-info>
           #:<cfp-page>

           ;; constants
           #:+entity-cat-conference+
           ))
