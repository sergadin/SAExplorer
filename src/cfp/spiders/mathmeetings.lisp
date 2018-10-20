;;;;
;;;; https://mathmeetings.net/
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.mathmeetings
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.mathmeetings)

(defclass <mathmeetings> (<cfp-spider>)
  ()
  (:default-initargs :name "mathmeetings"))

(alexandria:define-constant +rss-url+ "https://mathmeetings.net/conferences/index.rss" :test #'string=)

(alexandria:define-constant +dates-location-regex+
  "(\\d{4}-\\d{2}-\\d{2}) through (\\d{4}-\\d{2}-\\d{2}), (.+)"
  :test #'string=
  :documentation "Regex to extract start date, end date and location from mathmeeting RSS description field")

(defun parse-dates (rss-item)
  (ppcre:register-groups-bind (start-date end-date location)
      (+dates-location-regex+ (rss:description rss-item))
    (declare (ignore location))
    (list start-date end-date)))

(defun parse-location (rss-item)
  (ppcre:register-groups-bind (start-date end-date location)
      (+dates-location-regex+ (rss:description rss-item))
    (declare (ignore start-date end-date))
    location))

(defmethod cfp-collect ((spider <mathmeetings>))
  (let* ((rss-channel (rss:parse-rss-stream (sa-utils:fetch-url +rss-url+)))
         cfps)
    (dolist (item (rss:items rss-channel))
      (push (make-cfp-reference-info
        :name (rss:title item)
        :dates (format nil "~{~A - ~A~}" (parse-dates item))
        :location (parse-location item)
        :url (rss:link item))
      cfps))
    cfps))

(eval-when (:load-toplevel)
  (register-spider (make-instance '<mathmeetings>)))
