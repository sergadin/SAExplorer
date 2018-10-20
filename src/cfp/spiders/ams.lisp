;;;;
;;;; https://www.ams.org/meetings/calendar/mathcal
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.ams
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.ams)

(defclass <ams> (<cfp-spider>)
  ()
  (:default-initargs :name "ams"))

(alexandria:define-constant +url-template+ "https://www.ams.org/cgi-bin/mathcal/mathcalendar.pl?select_year=~A" :test #'string=)

(defmethod cfp-collect ((spider <ams>))
  (dolist (year '(2019 2020))
  (let* ((url (format nil +url-template+ year))
         (doc-root (plump:parse (sa-utils:fetch-url url))))
    (loop :for event-node :across (clss:select ".event" doc-root)
       collect (make-cfp-reference-info
         :name (plump:text (first (coerce (clss:select ".event_title" event-node) 'list)))
         :dates (string-trim '(#\Space #\Newline) (plump:text (first (coerce (clss:select ".event_dates" event-node) 'list))))
         :location (plump:text (first (coerce (clss:select ".event_location" event-node) 'list)))
         :url (plump:attribute (first (coerce (clss:select ".event_url > a" event-node) 'list)) "href"))))))

(eval-when (:load-toplevel)
  (register-spider (make-instance '<ams>)))
