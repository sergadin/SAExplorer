;;;;
;;;; https://www.acm.org/conferences/conference-events
;;;; https://www.acm.org/conferences/non-acm-events
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.acm
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.acm)

(defclass <acm> (<cfp-spider>)
  ()
  (:default-initargs :name "acm"))

(alexandria:define-constant +conference-events-url+ "https://www.acm.org/conferences/conference-events?view0=month&startDate0=~A-~2,'0d-~2,'0d" :test #'string=)
(alexandria:define-constant +non-acm-events-url+ "https://www.acm.org/conferences/non-acm-events?view0=month&startDate0=~A-~2,'0d-~2,'0d" :test #'string=)
(defconstant +year+ 2019)

(defmethod cfp-collect ((spider <acm>))
  (let ((conferences-data (make-hash-table :test 'equal)))
    (dolist (url-template (list +conference-events-url+ +non-acm-events-url+))
      (dolist (month '(1 2 3 4 5 6 7 8 9 10 11 12))
        (let* ((url (format nil url-template +year+ month 1))
               (doc-root (plump:parse (sa-utils:fetch-url url))))
          (loop :for day-node :across (clss:select "li.day:not(.other-month)" doc-root)
             :for date-node = (first (coerce (clss:select "div.date" day-node) 'list))
             :for clean-day = (parse-integer (plump:text date-node))
             :for date = (format nil "~A-~2,'0d-~2,'0d" +year+ month clean-day)
             :do
               (loop :for link :across (clss:select "div.event-desc > a" day-node)
                  :for conference-name = (plump:attribute link "title")
                  :for conference-url = (plump:attribute link "href")
                  :for conference-data = (list :url conference-url :start-date date :end-date date)
                  :for hash-entry = (gethash conference-name conferences-data)
                  :do
                    (if hash-entry
                      (setf (getf hash-entry :end-date) date)
                      (setf (gethash conference-name conferences-data) conference-data)))))))
    (loop for conference-name being the hash-keys of conferences-data
       using (hash-value conference-data)
       collect (make-cfp-reference-info
           :name conference-name
           :dates (format nil "~A - ~A" (getf conference-data :start-date) (getf conference-data :end-date))
           :url (getf conference-data :url)))))

(eval-when (:load-toplevel)
  (register-spider (make-instance '<acm>)))
