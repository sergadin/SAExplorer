;;;; http://www.doctorsreview.com/meetings/search/?page=7&start=2018-11-01

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.doctorsreview
  (:use :cl :saexplorer.cfp)
  (:import-from :saexplorer.cfp
                #:make-cfp-reference-info
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.doctorsreview)


(defparameter
    +url+
  "http://www.doctorsreview.com/meetings/search/?page=~D&start=~D-~2,'0D-~2,'0D"
  "URL template string that accepts 1-based page index, 4-digit year,
  1-based month index, and 2-digit day in that order.")

(alexandria:define-constant +maximum-number-of-pages+ 100
  :documentation "A constant to avoid infinite loop. Spider will quit
  after reachong thos limit.")

(defvar *cached* (make-hash-table :test #'equal))

(defclass <doctors-review> (<cfp-spider>)
  ()
  (:default-initargs :name "doctorsreview"))


(defun safe-first (arr)
  "Extract first element from the array ARR, or return NIL, if ARR is not an array, or empty."
  (when (and (arrayp arr) (= 1 (array-rank arr)) (< 0 (length arr)))
    (aref arr 0)))

(defun has-next-page (parsed-page)
  "Check that page contains a link to the next page."
  (some #'(lambda (ref)
            (not (null (search "NEXT" (plump:text ref)))))
        (clss:select "a[href^=\"?page=\"" parsed-page)))

(defun cached-fetch-url (url)
  "A replacement of `sa-utils:fetch-url' that allows to cache HTML code in special variable *cached*."
  (let ((html (gethash url *cached*)))
    (or html
        (setf (gethash url *cached*) (sa-utils:fetch-url url)))))

(defmethod cfp-collect ((spider <doctors-review>))
  (loop
     :with today = (local-time:today)
     :with minimal-year = (local-time:timestamp-year today)
     :and minimal-month = (local-time:timestamp-month today)
     :and minimal-day = (local-time:timestamp-day today)
     :for page-number :from 1
     :for url = (format nil +url+ page-number minimal-year minimal-month minimal-day)
     :for doc-root = (plump:parse (sa-utils:fetch-url url))
     :append
     (loop
        ;; The inner loop processes single page
        :for title-node :across (clss:select "div[id=\"meeting_results\"] > h3 > a[href^=\"/meetings/\"" doc-root)
        :for description-node = (plump:next-sibling (plump:next-sibling (plump:parent title-node)))
        :for dates = (safe-first (clss:select "span[class~=\"date\"" description-node))
        :for location-country = (safe-first (clss:select "span[class~=\"location\"] a[href^=\"/destination/\"" description-node))
        :for location-city = (safe-first (clss:select "span[class~=\"location\"] a[href^=\"/city/\"" description-node))
        :for classified = (loop
                             :with ht = (make-hash-table :test #'equal)
                             :for nd = (plump:next-sibling (plump:parent title-node)) :then (plump:next-sibling nd)
                             :while (or (plump:text-node-p nd)
                                        (member (plump:tag-name nd) '("p" "span") :test #'string-equal))
                             :do (when (and (plump:element-p nd)
                                            (plump:has-attribute nd "class"))
                                   (setf (gethash (plump:attribute nd "class") ht) nd))
                             :finally (return ht))
        :collect
        (make-cfp-reference-info
         :name (plump:text title-node)
         :dates (when dates (plump:text dates))
         :location  (format nil "~@[~A,~] ~@[~A~]"
                            (when location-city (plump:text location-city))
                            (when location-country (plump:text location-country)))
         :url (let* ((link-node (gethash "website" classified nil))
                     (a-node (when link-node (safe-first (clss:select "a" link-node)))))
                (when a-node (plump:attribute a-node "href")))))
     :while (and (< page-number +maximum-number-of-pages+)
                 (has-next-page doc-root))))


(eval-when (:load-toplevel)
  (register-spider (make-instance '<doctors-review>)))
