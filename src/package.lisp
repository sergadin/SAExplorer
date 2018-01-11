(defpackage :saexplorer.sys
  (:use :cl)
  (:export #:*dbcon* #:*config*
           #:generic-error))

(defpackage :saexplorer
  (:use :cl :saexplorer.sys)
  (:import-from :cl-log :log-message)
  (:import-from #:alexandria
                #:define-constant
                #:when-let)
  (:export #:main))
