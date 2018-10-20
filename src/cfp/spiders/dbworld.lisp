;;;; Extract references from DBWorld


(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.dbworld
  (:use :cl :saexplorer.cfp)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.dbworld)


(defparameter +url+ "https://research.cs.wisc.edu/dbworld/browse.html")
(defvar *cached* nil)

(defclass <dbworld> (<cfp-spider>)
  ()
  (:default-initargs :name "dbworld"))


(defconstant +message-type-col+ 2 "Manually assigned type of the message.")
(defconstant +title-col+ 4 "Position of message title, presumably, a conference name.")
(defconstant +deadline-col+ 5 "Position of the deadline.")
(defconstant +url-col+ 6 "Position of the external link.")


(defun get-td (node n &optional sub-selector)
  "Extract N-th td element of the NODE, that can any node."
  (flet ((safe-first (arr)
           (when (and (arrayp arr) (= 1 (array-rank arr)) (< 0 (length arr)))
             (aref arr 0))))
    (let* ((selector (format nil "td:nth-child(~D)" n))
           (tds (clss:select selector node)))
      (alexandria:when-let
          ((td (safe-first tds)))
        (if sub-selector
            (safe-first (clss:select sub-selector td))
            td)))))

(defmethod cfp-collect ((spider <dbworld>))
  (unless *cached*
    (setf *cached* (sa-utils:fetch-url +url+)))
  (loop
     :with doc-root = (plump:parse *cached*)
     :for cfp :across (clss:select "tbody > tr" doc-root)
     :for title-td = (get-td cfp +title-col+)
     :and source-url = (get-td cfp +title-col+ "a[href]")
     :and url-node = (get-td cfp +url-col+ "a[href]")
     :and deadline-td  = (get-td cfp +deadline-col+)
     :and message-type-td = (get-td cfp +message-type-col+)
     :when (and deadline-td url-node message-type-td
                (not (null (search "conf. ann."
                                   (string-downcase (plump:text message-type-td))))))
     :collect
     (make-cfp-reference-info
      :name (plump:text title-td)
      :deadline (when deadline-td (plump:text deadline-td))
      :source-url (when source-url (plump:attribute source-url "href"))
      :url (plump:attribute url-node "href"))))
  nil)


(eval-when (:load-toplevel)
  (register-spider (make-instance '<dbworld>)))
