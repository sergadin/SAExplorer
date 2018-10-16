(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.wikicfp
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.wikicfp)

(defclass <wikicfp> (<cfp-spider>)
  ()
  (:default-initargs :name "wikicfp"))

(defconstant +title-location-dates-regex+
  "(.*?)\\W+\\[(.*?)\\]\\W*\\[(.*?)\\]"
  "Regex to extract title, location and dates from WikiCFP RSS description field")

(defun parse-location (rss-item)
  (ppcre:register-groups-bind (title location dates)
      (+title-location-dates-regex+ (rss:description rss-item))
    (declare (ignore title dates))
    location))

(defun parse-dates (rss-item)
  (ppcre:register-groups-bind (title location dates)
      (+title-location-dates-regex+ (rss:description rss-item))
    (declare (ignore title location))
    dates))

(defun parse-title (rss-item)
  (ppcre:register-groups-bind (title location dates)
      (+title-location-dates-regex+ (rss:description rss-item))
    (declare (ignore dates location))
    (string-trim " " title)))


(defun parse-acronym (rss-item)
  (let ((descr-title (parse-title rss-item)))
    (declare (ignore descr-title))
    (ppcre:register-groups-bind (acronym title)
        ("(.*?)\\W+:\\W+(.*)" (rss:title rss-item))
      (declare (ignore title))
      acronym)))



(defmethod cfp-collect ((spider <wikicfp>))
  (let* ((url-wikicfp "http://www.wikicfp.com/cfp/rss")
         (rss-channel (rss:parse-rss-stream
                       (sa-utils:fetch-url url-wikicfp)))
         cfps)
    (dolist (item (rss:items rss-channel))
      (push (make-cfp-reference-info
             :name (parse-title item)
             :location (parse-location item)
             :acronym (parse-acronym item)
             :dates (parse-dates item)
             :url (rss:link item))
            cfps))
    cfps))


(eval-when (:load-toplevel)
  (register-spider (make-instance '<wikicfp>)))
