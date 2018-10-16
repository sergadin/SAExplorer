;;;;
;;;; http://www.globaleventslist.elsevier.com/rss.aspx?eventTypeIds=1,9,5&filterDates=2019-01
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.elsevier
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.elsevier)

(defclass <elsevier> (<cfp-spider>)
  ()
  (:default-initargs :name "elsevier"))


;; 2018-02
(defvar +by-month-url-template+ "http://www.globaleventslist.elsevier.com/rss.aspx?eventTypeIds=1,9,5&filterDates=~A-~2,'0D")


(defun parse-dates (rss-item)
  (ppcre:register-groups-bind (dates town country)
      ("(.*),\\W+(.*),\\W(.*)" (rss:description rss-item))
    (declare (ignore town country))
    (string-trim " " dates)))

(defun parse-location (rss-item)
  (print (rss:description rss-item))
  (ppcre:register-groups-bind (dates location)
      ("([^,]*),\\W+(.*)" (rss:description rss-item))
    (declare (ignore dates))
    (string-trim " " location)))



(defun make-url (year month)
  (format nil +by-month-url-template+ year month))


(defmethod cfp-collect ((spider <elsevier>))
  (let (cfps)
    (dolist (year '(2019 2020 2021))
      (dolist (month '(1 2 3 4 5 6 7 8 9 10 11 12))
        (let* ((url (make-url year month))
               (rss-channel (rss:parse-rss-stream
                             (sa-utils:fetch-url url))))
          (dolist (item (rss:items rss-channel))
            (push (make-cfp-reference-info
                   :name (rss:title item)
                   :location (parse-location item)
                   :dates (parse-dates item)
                   :url (rss:link item))
                  cfps)))))
    ; (format t "FEED created: ~A~%" (rss:pub-date rss-channel))
    cfps))


(eval-when (:load-toplevel)
  (register-spider (make-instance '<elsevier>)))
