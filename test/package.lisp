(in-package :cl-user)

(defpackage :saexplorer-test
  (:use :cl :lift :saexplorer)
  (:export #:run-all-tests)
  (:documentation
   "This package contains unit tests of SAExplorer."))

(in-package :saexplorer-test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite."))


(defun run-all-tests ()
  (lift:run-tests :suite 'root :break-on-errors? nil))
