;;;;
;;;; Access to aminer.org, an open-access database that covers >150M documents.
;;;; http://doc.aminer.org/en/latest/s/index.html
;;;;

(in-package :cl-user)

(defpackage saexplorer.aminer
  (:nicknames :aminer)
  (:use :cl :saexplorer.bibsystem)
  (:import-from :saexplorer #:get-json-item)
  (:import-from :cl-log
                #:log-message))

(in-package :saexplorer.aminer)

(define-bibsystem ("AMiner")
    :accept-encodings '(:json "application/json"))

;; Do not use proxy, as it requires authentication on the AMiner site.
(defmethod bibsys::proxy-data ((system <aminer>))
  (declare (ignore system))
  nil)

(defmethod rest-endpoint ((system <aminer>) (query <publ-search-query>) &key format)
  (declare (ignore system query format))
  "https://api.aminer.org/api/search/pub/advanced")

(defmethod rest-endpoint ((system <aminer>) (query <author-search-query>) &key format)
  (declare (ignore system query format))
  "https://api.aminer.org/api/search/person/advanced")

;;;
;;; Queries formatting
;;;

(defun extract-term-name-affiliation (query)
  (destructuring-bind (op field term)
      (query-filters query)
    (declare (ignore op field))
    (values term nil nil)))

(defmethod bibsys::rest-query-parameters
    ((system <aminer>) (query <publ-search-query>) start chunk-size &key format facets)
  (declare (ignore system format facets))
  ;; unused
  ;; sorting types: relevance, n_pubs, n_citation, diversity, activity, rising_star, new_star
  (multiple-value-bind (term author-name organization-name)
      (extract-term-name-affiliation query)
    `(("term" . ,term)
      ,@(when author-name `("name" ,author-name))
      ,@(when organization-name `("org" ,organization-name))
      ("size" . ,(format nil "~D" chunk-size))
      ("offset" . ,(format nil "~D" start))
      ("sort" . "relevance"))))

(defmethod bibsys::rest-query-parameters
    ((system <aminer>) (query <author-search-query>) start chunk-size &key format)
  ""
  (declare (ignore system format))
  (list (cons "term" query)
        (cons "size" (string chunk-size))
        (cons "offset" (string start))))

;;;
;;; Parsing responses
;;;

(defun parse-entry (entry-json)
  entry-json)

(defmethod bibsys:parse-response ((system <aminer>) content (format (eql :json)) &key result-object)
  (declare (ignore system))
  (let* ((json (cl-json:decode-json-from-string content))
         (total-results (get-json-item json '(:total)))
         (entries (get-json-item json '(:result)))
         (result (or result-object (make-instance '<aminer-result> :total-results total-results))))
    (setf (bibsys:entries result) (append (bibsys:entries result)
                                          (mapcar #'parse-entry entries)))
    result))


(defun self-test ()
  (let ((q (make-instance '<publ-search-query>)))
    (bibsys::query (bibsys::find-system "AMiner")
                   (bibsys::make-simple-query q "abac")
                   :max-results 500
                   :format :json)))
