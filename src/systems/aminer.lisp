;;;;
;;;; Access to aminer.org, an open-access database that covers >150M documents.
;;;; http://doc.aminer.org/en/latest/s/index.html
;;;;

(in-package :cl-user)

(defpackage saexplorer.aminer
  (:nicknames :aminer)
  (:use :cl :saexplorer.bibsystem)
  (:import-from :cl-log
                #:log-message))

(in-package :saexplorer.aminer)

(define-bibsystem ("AMiner")
    :accept-encodings '(:json "application/json"))

;; Do not use proxy, as it requires authentication on the AMiner site.
(defmethod bibsys::proxy-data ((system <aminer>))
  (declare (ignore system))
  nil)

(defmethod bibsys::rest-endpoint ((system <aminer>) (query <publ-search-query>) &key format)
  (declare (ignore system query format))
  "https://api.aminer.org/api/search/pub/advanced")

(defmethod bibsys::rest-endpoint ((system <aminer>) (query <author-search-query>) &key format)
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
  (declare (ignore system format))
  (list (cons "term" query)
        (cons "size" (string chunk-size))
        (cons "offset" (string start))))

;;;
;;; Parsing responses
;;;   see https://aminer.org/open-academic-graph for the description of fields
;;;

(defparameter *aminer-sources*
  `(("dblp")
    ("msra")
    ("mag")
    ("acm")
    ("wos")
    ("scopus")
    ("ieee"))
  "Acronyms appearing in AMiner output as values of SRC field in the
  VERSIONS section.")

(defparameter *json-publication-getters*
  `((:title . "$.title")
    (:abstract . "$.abstract")
    (:authors . "$.authors")
    (:keywords . "$.keywords")
    (:lang . "$.lang")
    (:start-page . "$.pages.s")
    (:end-page . "$.pages.e")
    (:year . "$.year")
    (:venue-name . "$.venue.name")
    (:doi . "$.doi")
    (:fulltext-url . "$.pdf"))
  "Map attributes names to JSONPath expressions used to extract the given attribute from parsed publication.")

(defun extract-ids (entry-json)
  "Extract known versions of this entry. Result is a list of triplets (aminer-id source
source-id), where source is AMiner specific code of information source. aminer-ids are
all different, while others may appear more then once."
  (loop for info in (jsonpath:match entry-json "$.versions")
     collect (mapcar #'(lambda (jp) (jsonpath:match info jp)) '("$.id" "$.src" "$.sid"))))


(defun parse-entry-json (entry-json)
  (let ((ids-for-entry (extract-ids entry-json))
        (aminer (bibsys:find-system "AMiner")))
    ;;(log-message :info "~A" ids-for-entry)
    (make-instance 'bibsys:<publication-document>
                   :identifier (bibsys:make-identifier (caar ids-for-entry) aminer)
                   :source-system aminer
                   :content entry-json :format :json
                   :getters (make-jsonpath-getter *json-publication-getters*))))

(defmethod bibsys:parse-response ((system <aminer>) content (format (eql :json)) &key result-object)
  (declare (ignore system))
  (let* ((json (cl-json:decode-json-from-string content))
         (total-results (jsonpath:match json "$.total"))
         (entries (jsonpath:match json "$.result"))
         (result (or result-object (make-instance '<aminer-result> :total-results total-results))))
    (setf (bibsys:entries result) (append (bibsys:entries result)
                                          (mapcar #'parse-entry-json entries)))
    result))


(defun self-test ()
  (let ((q (make-instance '<publ-search-query>)))
    (loop for doc in (bibsys:entries (bibsys::query (bibsys:find-system "AMiner")
                                                    (bibsys::make-simple-query q "abac")
                                                    :max-results 20
                                                    :format :json))
       do (log-message :info "~A" (bibsys::document-property doc :title)))))
