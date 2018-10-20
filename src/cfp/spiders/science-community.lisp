;;;; Extract references from https://www.science-community.org

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.science-community
  (:use :cl :saexplorer.cfp)
  (:import-from :spider-utils
                #:safe-first))

(in-package :saexplorer.cfp.spider.science-community)


(defclass <science-community> (saexplorer.cfp::<cfp-spider>)
  ()
  (:default-initargs :name "science-community.org"))

(defclass <depaginator> (spider-utils:<depaginator>)
  ()
  (:default-initargs
   :initial-number 0))


(defmethod spider-utils::initial-pages
    ((depaginator <depaginator>))
  (mapcar
   #'(lambda (topic)
       (format nil "https://www.science-community.org/en/conferences/0/0/~A?filterby=deadline&page=~~D" topic))
    '("Medicine" "Physics-and-math")))

(defmethod spider-utils::page-url
    ((depaginator <depaginator>)
     url-template
     page-number)
  (format nil url-template page-number))

(defmethod spider-utils::process-single-page
    ((depaginator <depaginator>)
     page-number)
  nil)

(defun absolute-url (href)
  (if (eql 0 (search "/" href))
    (concatenate 'string "https://www.science-community.org" href)
    href))


(defmethod spider-utils::process-single-page
    ((depaginator <depaginator>)
     doc-root)
  (loop
     :for cfp :across (clss:select "div[class~=\"view-content\"] > div[class~=\"views-row\"]" doc-root)
     :for title-a = (safe-first (clss:select "div[class~=\"views-field-title\"] a[href]" cfp))
     :and location-node = (safe-first (clss:select "div[class~=\"views-field-field-city\"] div[class~=\"field-content\"]" cfp))
     :and dates = (safe-first (clss:select "div[class~=\"views-field-field-conf-start\"] span[class~=\"date-display-single\"]" cfp))
     :and deadline = (safe-first (clss:select "div[class~=\"views-field-field-conference-theses-deadlin\"] span[class~=\"date-display-single\"]" cfp))
     :when (and title-a dates)
     :collect
     (saexplorer.cfp::make-cfp-reference-info
      :name (print (string-trim '(#\Space #\Newline #\Tab) (plump:text title-a)))
      :source-url (absolute-url (plump:attribute title-a "href"))
      :dates (plump:text dates)
      :location (when location-node (plump:text location-node))
      :deadline (when deadline (plump:text deadline))))
  nil)

(defmethod spider-utils::next-url ((depaginator <depaginator>) doc-root)
  (safe-first (clss:select "ul[class~=\"pager\"] li[class~=\"pager-next\"]" doc-root)))



(defmethod saexplorer.cfp::cfp-collect ((spider <science-community>))
  (let ((depaginator (make-instance '<depaginator>)))
    (spider-utils::process depaginator)))


(eval-when (:load-toplevel)
  (saexplorer.cfp::register-spider (make-instance '<science-community>)))
