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


(defun make-url (year month)
  (format nil +by-month-url-template+ year month))


(defmethod cfp-collect ((spider <elsevier>))
  (let* ((url (make-url 2019 02))
         (rss-channel (rss:parse-rss-stream
                       (sa-utils:fetch-url url)))
         (cfps))
    (dolist (item (rss:items rss-channel))
      (push (make-cfp-reference-info
             :name (rss:title item)
             :location nil
             :dates (rss:description item)
             :url (rss:link item))
            cfps))
    ; (format t "FEED created: ~A~%" (rss:pub-date rss-channel))
    cfps))


(eval-when (:load-toplevel)
  (register-spider (make-instance '<elsevier>)))
