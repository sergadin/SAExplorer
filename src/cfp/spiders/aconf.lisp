;;;;
;;;; https://www.aconf.org/
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.aconf
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.aconf)

(defclass <aconf> (<cfp-spider>)
  ()
  (:default-initargs :name "aconf.org"))

(alexandria:define-constant +url-prefix+ "https://www.aconf.org" :test #'string=)


(clss:define-pseudo-selector aconf-conf-link (node)
  (let ((href (plump:attribute node "href"))
        (target (plump:attribute node "target")))
    (and href (cl-ppcre:scan "^/conf_[0-9]+.html" href)
         target (cl-ppcre:scan "^_blank" target))))

(defun parse-dates (dates-and-place-string)
  (first (cl-ppcre:all-matches-as-strings "[0-9\-]{4,10}.{0,3}[0-9\-]{2,10}" dates-and-place-string)))

(defun parse-location (dates-and-place-string)
  (first (cl-ppcre:all-matches-as-strings "[A-Z].*$" dates-and-place-string)))


(defmethod cfp-collect ((spider <aconf>))
  (let* (cfps
         (url (format nil "~A/" +url-prefix+))
         (doc-root (plump:parse (sa-utils:fetch-url url))))
    (loop
       :for event-node :across (clss:select "a:aconf-conf-link" doc-root)
       :for date-place-string = (plump:text (first (coerce (clss:select "p" event-node) 'list)))
       :do
         (push (make-cfp-reference-info
                :name (plump:text (first (coerce (clss:select "h2" event-node) 'list)))
                :dates (parse-dates date-place-string)
                :location (parse-location date-place-string)
                :url (format nil "~A/~A" +url-prefix+ (plump:attribute event-node "href")))
               cfps))
    cfps))

(eval-when (:load-toplevel)
  (register-spider (make-instance '<aconf>)))
