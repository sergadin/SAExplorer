;;; Function for quering Scopus API.

(in-package :cl-user)

(defpackage saexplorer.scopus
  (:nicknames :scopus)
  (:use :cl :saexplorer.bibsystem)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :cl-log
                #:log-message)
  (:import-from :saexplorer
                #:split-keywords)
  (:import-from :saexplorer.bibsystem
                #:<bibliography-system>
                #:<result>
                #:rest-query-parameters #:rest-query-headers))

(in-package :saexplorer.scopus)

(defparameter *default-api-host* "https://api.elsevier.com" "Scopus REST API host and protocol.")
(defparameter *search-endpoint-url* "content/search/scopus" "URL, without the leading '/', to submit search requests.")
(defparameter *authors-endpoint-url* "content/search/authors" "URL, without the leading '/', to submit author search requests.")


(defparameter *available-facets-names*
  '("exactsrctitle" "au-id" "authname" "pubyear" "subjarea" "language" "af-id" "exactkeyword" "srctype" "country" "aucite")
  "List of facets numbers provided by Scopus.")


(define-bibsystem ("Scopus")
    :accept-encodings '(:json "application/json"
                        :xml+atom "text/xml, application/atom+xml"))


(defmethod bibsys::rest-endpoint ((system <scopus>) query &key format)
  (declare (ignore system query format))
  (format nil "~A/~A" (bibsys::config-option system "api-host") *search-endpoint-url*))


(defmethod bibsys::rest-endpoint ((system <scopus>) (query <author-search-query>) &key format)
  (declare (ignore system query format))
  (format nil "~A/~A" (bibsys::config-option system "api-host") *authors-endpoint-url*))



(defun format-facets-request (facets-names &key (count 10) (sort "fdna"))
  "sort : how the navigators should be sorted. Options include
  na (Modifier name, ascending), fd (Modifier frequency, descending),
  and fdna (Modifier frequency descending, secondary sort through
  unity by name, ascending).
"
  (let ((format-string (format nil "~~{~~A(count=~D,sort=~A);~~}" count sort)))
    (format nil format-string facets-names)))

(defun proximity (kw)
  "Encode a string using Scopus proximity search notation, e.g. A w/0 B."
  (format nil "~{~A~^ w/0 ~}" (split-keywords kw :delimiter #\Space)))


(defgeneric convert (query)
  (:documentation "Represent QUERY using Scopus query language."))

(defmethod convert ((query string))
  query)

(defmethod convert ((query <search-query>))
  (let ((filters (bibsys:query-filters query)))
    (car (last filters))))


(defmethod rest-query-parameters ((system <scopus>) (query <publ-search-query>) start chunk-size &key format)
  (declare (ignore system format))
  (list (cons "query" (convert query))
        ;;(cons "fields" fields)
        (cons "facets" (when (zerop start)
                         (format-facets-request *available-facets-names* :count 30)))
        (cons "view" "complete")
        (cons "count" (format nil "~D" chunk-size))
        (cons "start" (format nil "~D" start))))

(defmethod rest-query-headers ((system <scopus>) query &key format facets)
  (declare (ignore system format facets))
  `(("X-ELS-APIKey" . ,(bibsys:config-option system "api-key"))
    ("X-ELS-ResourceVersion" . "XOCS")))


;;;
;;; Parse response
;;;

(defparameter *scopus-json-publication-getters*
  `((:id . "$.dc:identifier")
    (:title . "$.dc:title")
    (:abstract . "$.abstract")
    (:authors . "$.authors")
    (:keywords . "$.authkeywords")
    (:lang . nil)
    (:start-page . "$.prism:page-range")
    (:end-page . "$.prism:page-range")
    (:year . "$.prism:cover-date")
    (:venue-name . "$.prism:publication-name")
    (:doi . "$.prism:doi")
    (:fulltext-url . nil)))


(defun parse-entry (entry-json)
  (let ((scopus (bibsys:find-system "Scopus")))
    (make-instance 'bibsys:<publication-document>
                   :identifier (bibsys:make-identifier "123" scopus)
                   :source-system scopus
                   :content entry-json :format :json
                   :getters (bibsys:make-jsonpath-getter *scopus-json-publication-getters*))))

(defun ensure-multiple-categories (category-data)
  "Facet data format differes for single-item and multiple items
categories. Single-item facets are represented as
 (:category (:name . ...) (:value ...) ...),  while normal facets are
 (:category ((:name . ...) ...) (...)).

This function convers category-data to the multi-items form.
"
  (when category-data
    (if (and (consp (first category-data)) (consp (caar category-data)))
        category-data
        (list category-data))))

(defun polish-facet-items (facet-name extracted-data)
  "Perform post-processing on facet data extracted from Scopus
response. For example, Scopus encodes authorId inside the name field
of by-author facet using # delimiter."
  (alexandria:switch (facet-name :test #'string-equal)
    ("authname" (mapcar (lambda (item)
                          (destructuring-bind (name . id)
                              (split-sequence #\# (getf item :name))
                            `(:name ,name :value ,(first id) :hit-count ,(getf item :hit-count))))
                        extracted-data))
    (t extracted-data)))


(defmethod bibsys:parse-response ((system <scopus>) content (format (eql :json)) &key result-object)
  "Parse JSON document returned by Scopus search engine."
  (declare (ignore system))
  (let* ((json (cl-json:decode-json-from-string content))
         (total-results (parse-integer (jsonpath:match json "$.search-results.opensearch:total-results")))
         (facets (jsonpath:match json "$.search-results.facet"))
         (entries (jsonpath:match json "$.search-results.entry"))
         (result (or result-object (make-instance '<scopus-result> :total-results total-results))))
    ;; Assign facet data
    (when (or (not (slot-boundp result 'bibsys:facets))
              (null (bibsys:facets result)))
      (loop :for name-attribute-category :in facets
         :for name = (cdr (assoc :name name-attribute-category))
         :and category = (ensure-multiple-categories (rest (assoc :category name-attribute-category)))
         :for facet = (make-instance 'bibsys:<facet> :name name)
         :do
         (setf (bibsys:facet-items facet)
               (polish-facet-items
                name
                (mapcar (lambda (category-data)
                          (mapcan (lambda (key) (list key (cdr (assoc key category-data))))
                                  '(:name :value :hit-count)))
                        category)))
         (push facet (bibsys:facets result))))
    ;; Extend matched entries
    (setf (bibsys:entries result) (append (bibsys:entries result)
                                          (mapcar #'parse-entry entries)))
    result))

(defmethod bibsys:parse-response ((system <scopus>) content (format (eql :xml+atom)) &key result-object)
  (declare (ignore system))
  (let* ((document (cxml:parse content (cxml-stp:make-builder)))
         (nodes (stp:filter-recursively (stp:of-name "totalResults"
                                                     "http://a9.com/-/spec/opensearch/1.1/")
                                        document))
         (total-results (parse-integer (stp:string-value (car nodes))))
         (result (or result-object (make-instance '<scopus-result> :total-results total-results))))
    result))




;; ;; (query-scopus "af-id(60007457) OR af-id(60073864) OR af-id(60075950) OR af-id(60068672)")
;; (defun query-scopus (query &key chunk-processor
;;                              (format :json)
;;                              (max-results 500))
;;   "Evaluate search query on Scopus. A query is given as a string in Scopus notation.

;; In response to a search query Scopus sends a sequence of matching
;; entries, devided into chunks. Results are processed either by chunks,
;; as far as new data become available, or after termination of this
;; function.

;; If CHUNK-PROCESOR is NIL the list of all documents are stored in
;; memory and then returned from the function. If CHUNK-PROCESSOR is
;; non-NIL, then it is should a reference to a function of two arguments,
;; starting number and document content. When new chunk arrives,
;; CHUNK-PROCESSOR is fired instead of collecting the data.

;; FORMAT specifies document encoding. It is either :JSON, or :XML+ATOM.

;; MAX-RESULTS limit maximal number of entries to be acquired from
;; Scopus. If NIL, all results will be downloaded, up to a Scopus
;; predefined limit, if any.
;; "
;;   (let ((url (format nil "http://~A/~A" *scopus-api-host* *search-endpoint-url*))
;;         (fields "field=prism:url,identifier,dc:title,prism:publicationName,prism:issn,prism:isbn,prism:volume,prism:issueIdentifier,prism:pageRange,prism:coverDate,prism:doi,description,dc:creator,author,authkeywords,affiliation,article-number,pii,citedby-count")
;;         (facets '("exactsrctitle" "au-id" "authname" "pubyear" "subjarea" "language" "af-id" "exactkeyword" "srctype" "country" "aucite"))
;;         (headers (list (cons "X-ELS-APIKey" *scopus-api-key*)
;;                        (cons "X-ELS-ResourceVersion" "XOCS")))
;;         (accept-format (ecase format
;;                          (:json "application/json")
;;                          (:xml+atom "text/xml, application/atom+xml")))
;;         (chunk-size 20)
;;         total-results
;;         processed
;;         documents)
;;     (do ((start 0 processed))
;;         ((and processed total-results
;;               (>= processed (min total-results (or max-results total-results)))))
;;       (let* ((chunk-signature (request-signature query format start chunk-size))
;;              (cached-chunk (red:get chunk-signature))
;;              (chunk-content cached-chunk))
;;         ;;(red:del chunk-signature)
;;         (when (not (red:exists chunk-signature))
;;           ;; Access Scopus, cache the result
;;           (log-message :info "Cache key ~A not found." chunk-signature)
;;           (log-message :trace "Querying Scopus (~A). start=~D, total=~D." url start total-results)
;;           (multiple-value-bind (content status-code)
;;               (drakma:http-request url
;;                                    :external-format-out :utf-8
;;                                    :additional-headers headers
;;                                    :parameters (list (cons "query" query)
;;                                                      ;;(cons "fields" fields)
;;                                                      (cons "facets" (when (zerop start)
;;                                                                       (format-facets-request facets :count 30)))
;;                                                      (cons "view" "complete")
;;                                                      (cons "count" (format nil "~D" chunk-size))
;;                                                      (cons "start" (format nil "~D" start)))
;;                                    :accept accept-format)
;;             (when (= status-code +http-ok+)
;;               (setf chunk-content (flexi-streams:octets-to-string content :external-format :utf-8))
;;               (red:set chunk-signature chunk-content))))
;;         ;; extract total results from chunk-content
;;         (unless total-results
;;           (ecase format
;;             (:json
;;              (let ((document (cl-json:decode-json-from-string chunk-content)))
;;                (setf total-results (parse-integer (jsonpath:match document "$.search-results.opensearch:total-results")))))
;;             (:xml+atom
;;              (let* ((document (cxml:parse chunk-content (cxml-stp:make-builder)))
;;                     (nodes (stp:filter-recursively (stp:of-name "totalResults"
;;                                                                 "http://a9.com/-/spec/opensearch/1.1/")
;;                                                    document)))
;;                (setf total-results (parse-integer (stp:string-value (car nodes))))))))
;;         (if chunk-processor
;;             (funcall chunk-processor start chunk-content)
;;             (push chunk-content documents))
;;         (setf processed (+ (or processed 0) chunk-size))))
;;     (nreverse documents)))


;; (defun query-scopus-affiliation (affiliation-id &key chunk-processor)
;;   (let ((url (format nil "http://~A/content/affiliation/AFFILIATION_ID:~A" *scopus-api-host* affiliation-id))
;;         (headers (list (cons "X-ELS-APIKey" *scopus-api-key*)
;;                        (cons "X-ELS-ResourceVersion" "XOCS")))
;;         (chunk-size 2)
;;         total-results
;;         processed)
;;     (do ((start 0 processed))
;;         ((and processed total-results (>= processed total-results)))
;;       (cl-log:log-message :trace "Выполняется запрос к Scopus. start=~D, total=~D." start total-results)
;;       (cl-log:log-message :trace "~A" url)
;;       (sleep 2)
;;       (multiple-value-bind (content status)
;;           (drakma:http-request url
;;                                :external-format-out :utf-8
;;                                :additional-headers headers
;;                                :parameters (list ;;(cons "view" "DOCUMENTS")
;;                                                  (cons "count" (format nil "~D" chunk-size))
;;                                                  (cons "start" (format nil "~D" start)))
;;                                :accept "text/xml, application/atom+xml")
;;         (declare (ignore status))
;;         (unless total-results
;;           (let* ((document (cxml:parse content (cxml-stp:make-builder)))
;;                  (nodes (stp:filter-recursively (stp:of-name "documents")
;;                                                 document)))
;;             (print document)
;;             (setf total-results
;;                   (parse-integer (stp:attribute-value (car nodes) "total")))))
;;         (when chunk-processor
;;           (funcall chunk-processor start content))
;;         (setf processed (+ (or processed 0) chunk-size))))))

;; (defun get-element-text (elem)
;;   (stp:data (stp:first-child elem)))


;; (fare-memoization:define-memo-function get-abstract-retrieval (scopus-id)
;;   "Load metadata for an article identified by SCOPUS-ID."
;;   (let ((url (format nil "http://~A/content/abstract/scopus_id/~A" *scopus-api-host* scopus-id))
;;         (headers (list (cons "X-ELS-APIKey" *scopus-api-key*)
;;                        (cons "X-ELS-ResourceVersion" "XOCS"))))
;;     (multiple-value-bind (content status)
;;         (drakma:http-request url
;;                              :external-format-out :utf-8
;;                              :additional-headers headers
;;                              :accept "text/xml, application/atom+xml, application/json")
;;       (declare (ignore status))
;;       content)))



#+(or)(defun abstract-retrieval (scopus-id)
  "Загрузка описания статьи."
  (let ((url (format nil "http://~A/content/abstract/scopus_id/~A" *scopus-api-host* scopus-id))
        (headers (list (cons "X-ELS-APIKey" *scopus-api-key*)
                       (cons "X-ELS-ResourceVersion" "XOCS"))))
    (multiple-value-bind (content status)
        (drakma:http-request url
                             :external-format-out :utf-8
                             :additional-headers headers
                             :accept "text/xml, application/atom+xml, application/json")
      (declare (ignore status))
      (let* ((document (cxml:parse content (cxml-stp:make-builder)))
             (bibliography (stp:filter-recursively (stp:of-name "bibliography")
                                                   document))
             (references (stp:filter-recursively (stp:of-name "ref-fulltext")
                                                 (first bibliography))))
        (dolist (ref references)
          (print (get-element-text ref)))))))
