;;;;
;;;; https://www.papercrowd.com/medicine-and-health/pharmaceutical-conferences-2018
;;;; TODO: generalize to load also other conferences on that website
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.papercrowd
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.papercrowd)

(defclass <papercrowd> (<cfp-spider>)
  ()
  (:default-initargs :name "papercrowd"))

(alexandria:define-constant +url+ "https://www.papercrowd.com/medicine-and-health/pharmaceutical/page" :test #'string=)

(defun absolute-url (href)
  (if (eql 0 (search "/" href))
    (concatenate 'string "https://www.papercrowd.com" href)
    href))

(defmethod cfp-collect ((spider <papercrowd>))
  (let* ((page-number 1))
    (loop
       :for page = (sa-utils:fetch-url +url+ :method :post :parameters `(("page" . ,(write-to-string page-number))))
       :while page
       :for doc-root = (plump:parse page)
       :do (format t "processing page ~A" page-number)
       :do (setf page-number (1+ page-number))
       :append
          (loop :for block :across (clss:select ".conference-summary-block" doc-root)
             :for acronym-node = (aref (clss:select ".conference-short-name" block) 0)
             :for name-node = (aref (clss:select ".conference-title" block) 0)
             :for location-node = (aref (clss:select ".location" block) 0)
             :for dates-node = (aref (clss:select ".conference-date:not(.location)" block) 0)
             :collect
               (make-cfp-reference-info
                 :name (plump:text name-node)
                 :acronym (plump:text acronym-node)
                 :source-url (absolute-url (plump:attribute (plump:parent block) "href"))
                 :dates (plump:text dates-node)
                 :location (plump:text location-node))))))

(eval-when (:load-toplevel)
  (register-spider (make-instance '<papercrowd>)))
