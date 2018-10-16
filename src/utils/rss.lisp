;;;;
;;;; RSS feed processor.
;;;;

(in-package :cl-user)

(defpackage saexplorer.rss
  (:use :cl :saexplorer.sys)
  (:import-from :saexplorer.utils
                #:fetch-url)
  (:import-from :cl-log
                #:log-message)
  (:export #:consume))

(in-package :saexplorer.rss)

;; Elsevier event types:
;; Conference, Symposium, Workshop: 1, 9, 5
;; http://www.globaleventslist.elsevier.com/search/

(defun consume ()
  (let* ((url-elsevier "http://www.globaleventslist.elsevier.com/rss.aspx?eventTypeIds=1,9,5")
         (url-ieee "http://events.vtools.ieee.org/meetings/rss")
         (url-chemistry "https://www.chemistry-conferences.com/conferences.xml")
         (url-wikicfp-cs "http://www.wikicfp.com/cfp/rss?cat=computer%20science")
         (url-wikicfp "http://www.wikicfp.com/cfp/rss")
         (rss-channel (rss:parse-rss-stream
                       (fetch-url url-chemistry :use-proxy nil))))
    (dolist (item (rss:items rss-channel))
      (format t "~A~%  ~A~%" (rss:title item) (rss:link item)))
    (format t "FEED created: ~A~%" (rss:pub-date rss-channel))))
