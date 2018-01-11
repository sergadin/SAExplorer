;;; Function for quering Scopus API.

(in-package :cl-user)

(defpackage saexplorer.dblp
  (:nicknames :dblp)
  (:use :cl)
  (:import-from :saexplorer.bibsystem
                #:<bibliography-system>
                #:<result>
                #:rest-endpoint-search)
  (:import-from :cl-log
                #:log-message))

(in-package :saexplorer.dblp)

(defparameter *dblp-api-host* "http://dblp.org")
(defparameter *search-endpoint-url* "search/publ/api" "URL, without the leading '/', to submit search queries.")

(defclass <dblp> (<bibliography-system>)
  ()
  (:default-initargs
   :name "DBLP"))

(defclass <dblp-result> (<result>)
  ()
  (:default-initargs
   :matched-entries nil))


(defmethod rest-endpoint-search ((system <dblp>))
  (format nil "~A/~A" *dblp-api-host* *search-endpoint-url*))


(defmethod bibsys::rest-query-parameters ((system <dblp>) query start chunk-size &key format facets)
  (declare (ignore system facets))
  `(("q" . ,query)
    ("h" . ,(format nil "~D" chunk-size))
    ("f" . ,(format nil "~D" start))
    ("format" . ,(ecase format
                        (:xml "xml")
                        (:json "json")
                        (:jsonp "jsonp")))))


(defmethod bibsys::parse-response ((system <dblp>) content (format (eql :xml)) &key result-object)
  (declare (ignore system))
  (let* ((document (cxml:parse content (cxml-stp:make-builder)))
         (total-results (xpath:number-value (xpath:evaluate "//hits/@total" document)))
         (result (or result-object (make-instance '<dblp-result> :total-results total-results))))
    (with-accessors ((entries bibsys::entries))
        result
      (push (xpath:all-nodes (xpath:evaluate "//hits/hit" document)) entries))
    result))



;; (defun query-dblp (query &key chunk-processor
;;                              (format :jsonp)
;;                              (max-results 500))
;;   "Evaluate search query on DBLP. A QUERY is as a string without any specific format.
;; "
;;   (let ((url (format nil "~A/~A" *dblp-api-host* *search-endpoint-url*))
;;         (chunk-size 20)
;;         total-results
;;         processed
;;         documents)
;;     (do ((start 0 processed))
;;         ((and processed total-results
;;               (>= processed (min total-results (or max-results total-results)))))
;;       (log-message :trace "Querying DBLP (~A). start=~D, total=~D." url start total-results)
;;       (multiple-value-bind (content status-code)
;;           (drakma:http-request url
;;                                :external-format-out :utf-8
;;                                :parameters (list (cons "q" query)
;;                                                  (cons "h" (format nil "~D" chunk-size))
;;                                                  (cons "f" (format nil "~D" start))
;;                                                  (cons "format" "xml"))
;;                                :accept "text/xml")
;;         (when (and (= status-code +http-ok+)
;;                    (not total-results))
;;           ;; extract total results
;;           (let* ((chunk-content content #+(or)(flexi-streams:octets-to-string content :external-format :utf-8))
;;                  (document (cxml:parse chunk-content (cxml-stp:make-builder))))
;;             (setf total-results (xpath:number-value (xpath:evaluate "//hits/@total" document)))
;;             (push document documents)))
;;       (setf processed (+ (or processed 0) chunk-size))))
;;     (nreverse documents)))


;;;
; get item ==> http://dblp.org/rec/xml/<KEY>.xml
;              http://dblp.org/rec/xml/journals/istr/PussewalageO17.xml
