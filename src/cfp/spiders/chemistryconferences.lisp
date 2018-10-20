;;;;
;;;; https://www.chemistryconferences.org/
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.chemistryconferences
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.chemistryconferences)

(defclass <chemistryconferences> (<cfp-spider>)
  ()
  (:default-initargs :name "chemistryconferences"))

(alexandria:define-constant +url+ "https://www.chemistryconferences.org/" :test #'string=)

(defmethod cfp-collect ((spider <chemistryconferences>))
  (let ((doc-root (plump:parse (sa-utils:fetch-url +url+))))
    (loop :for card-node :across (clss:select "#conf .card" doc-root)
      :for title-node = (first (coerce (clss:select ".card-title > a" card-node) 'list))
      :for date-node = (first (coerce (clss:select ".card-text > time" card-node) 'list))
      :for location-node = (first (coerce (clss:select ".card-text > strong" card-node) 'list))
      collect (make-cfp-reference-info
        :name (string-trim " " (plump:text title-node))
        :dates (string-trim " " (plump:attribute date-node "datetime"))
        :location (plump:text location-node)
        :url (plump:attribute title-node "href")))))

(eval-when (:load-toplevel)
  (register-spider (make-instance '<chemistryconferences>)))
