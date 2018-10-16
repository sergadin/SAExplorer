;;;;
;;;;  Collecting new URL references from
;;;;   https://www.omicsonline.org/international-scientific-conferences/
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.omicsonline
  (:use :cl :saexplorer.cfp)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.omicsonline)


(defparameter +url+ "https://www.omicsonline.org/international-scientific-conferences/")

(defvar *omicsonline* nil)

(defclass <omicsonline> (<cfp-spider>)
  ()
  (:default-initargs :name "omicsonline"))


(clss:define-pseudo-selector outside-link (node)
  (let ((href (plump:attribute node "href"))
        (target (plump:attribute node "target")))
    (and href (cl-ppcre:scan "^(http|https)://" href)
         target (cl-ppcre:scan "^_blank" target))))

(clss:define-pseudo-selector contains-date (node)
  (let ((text (plump:text node)))
    (and text
         (or (saexplorer.extract:all-dates text)
             (cl-ppcre:scan "(?i:January|February|March|April|May|June|July|August|September|October|November|December?)" text)))))


(defmethod cfp-collect ((spider <omicsonline>))
  (unless *omicsonline*
    (setf *omicsonline* (sa-utils:fetch-url +url+)))
  (let* ((doc-root (plump:parse *omicsonline*))
         (confs (clss:select "section.location-conf-main" doc-root)))
    (loop :for link :across (clss:select "a:outside-link" confs)
       :for dates-node = (first (coerce (clss:select "*:contains-date" link) 'list))
       :collect
             (make-cfp-reference-info
              :name (plump:attribute link "title")
              :dates (when dates-node (plump:text dates-node))
              :url (plump:attribute link "href")))))

(eval-when (:load-toplevel)
  (register-spider (make-instance '<omicsonline>)))
