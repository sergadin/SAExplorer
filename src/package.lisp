(defpackage :saexplorer.sys
  (:use :cl)
  (:export #:*dbcon* #:*config*
           #:generic-error #:invalid-request-error #:not-implemented-error
           #:saexplorer-error))

(defpackage :saexplorer
  (:use :cl :saexplorer.sys)
  (:import-from :cl-log :log-message)
  (:import-from #:alexandria
                #:define-constant
                #:when-let)
  (:export #:main))
