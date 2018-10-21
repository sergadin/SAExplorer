;;;; http://www.paperleap.com/cfp

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.paperleap
  (:use :cl :saexplorer.cfp)
  (:import-from :spider-utils
                #:safe-first))

(in-package :saexplorer.cfp.spider.paperleap)


(defclass <paperleap> (saexplorer.cfp::<cfp-spider>)
  ()
  (:default-initargs :name "paperleap"))

(defclass <depaginator> (spider-utils:<depaginator>)
  ()
  (:default-initargs
   :initial-number 0))


(defmethod spider-utils::initial-pages
    ((depaginator <depaginator>))
  ;; types=0 means Conference/workshop, types=1 means Journal/book
  '("http://www.paperleap.com/cfp?types=0&page=~A"))

(defmethod spider-utils::page-url
    ((depaginator <depaginator>)
     url-template
     page-number)
  (format nil url-template page-number))

(defmethod spider-utils::process-single-page
    ((depaginator <depaginator>)
     page-number)
  nil)

(defmethod spider-utils::process-single-page
    ((depaginator <depaginator>)
     doc-root)
  (loop
     :for cfp-node :across (clss:select ".positions-list-item" doc-root)
     :for acronym-node = (safe-first (clss:select "h2 > a" cfp-node))
     :for name-node = (safe-first (clss:select "h2 > strong > a.f13" cfp-node))
     :for dates-node = (safe-first (clss:select "h3" cfp-node))
     :for city-node = (safe-first (clss:select "a[href*=\"?cities=\"]" dates-node))
     :for country-node = (safe-first (clss:select "a[href*=\"?countries=\"]" dates-node))
     :for deadline-node = (safe-first (clss:select "div > div.pull-right > span" cfp-node))
     :collect
     (saexplorer.cfp::make-cfp-reference-info
       :name (plump:text name-node)
       :acronym (plump:text acronym-node)
       :source-url (plump:attribute name-node "href")
       :dates (string-right-trim '(#\, #\Space) (plump:text (plump:first-child dates-node)))
       :location (format nil "~A, ~A" (if city-node (plump:text city-node) "N/A") (if country-node (plump:text country-node) "N/A"))
       :deadline (plump:text (plump:first-child deadline-node)))))

(defmethod spider-utils::next-url ((depaginator <depaginator>) doc-root)
  (safe-first (clss:select ".row > .col-sm-8 > a.btn.btn-default.pull-right" doc-root)))



(defmethod saexplorer.cfp::cfp-collect ((spider <paperleap>))
  (let ((depaginator (make-instance '<depaginator>)))
    (spider-utils::process depaginator)))


(eval-when (:load-toplevel)
  (saexplorer.cfp::register-spider (make-instance '<paperleap>)))
