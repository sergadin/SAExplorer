;;;;
;;;;  Collecting new URL references from
;;;;   https://www.drugs.com/conferences/
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.drugs.com
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:<cfp-spider> #:cfp-collect #:register-spider)
  (:export #:collect))

(in-package :saexplorer.cfp.spider.drugs.com)


(defparameter +url+ "https://www.drugs.com/conferences/")

(defvar *drugscom* nil)

(defclass <drugs-com> (<cfp-spider>)
  ()
  (:default-initargs :name "drugs.com"))


#+(or)(clss:define-pseudo-selector drugs-com-dates (node)
  (let ((href (plump:attribute node "href"))
        (target (plump:attribute node "target")))
    (and href (cl-ppcre:scan "^(http|https)://" href)
         target (cl-ppcre:scan "^_blank" target))))

#+(or)(defun urls-to-fetch ()
  (let ((today (local-time:today)))
    (local-time:timestamp-year today)))



(defmethod cfp-collect ((spider <drugs-com>))
  (declare (ignore spider))
  (unless *drugscom*
    (setf *drugscom* (sa-utils:fetch-url +url+)))
  (let* ((doc-root (plump:parse *drugscom*))
         (confs (clss:select "section.location-conf-main" doc-root)))
    nil))


(eval-when (:load-toplevel)
  (register-spider (make-instance '<drugs-com>)))
