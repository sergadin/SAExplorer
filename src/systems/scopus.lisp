;;; Function for quering Scopus API.

(in-package :cl-user)

(defpackage saexplorer.scopus
  (:nicknames :scopus)
  (:use :cl :saexplorer.bibsystem)
  (:import-from :cl-log
                #:log-message)
  (:import-from :saexplorer
                #:split-keywords)
  (:import-from :saexplorer.bibsystem
                #:<bibliography-system>
                #:<result>
                #:rest-endpoint-search #:rest-query-parameters #:rest-query-headers))

(in-package :saexplorer.scopus)

(defparameter *search-endpoint-url* "content/search/scopus" "URL, without the leading '/', to submit search queries.")

(defparameter *available-facets-names*
  '("exactsrctitle" "au-id" "authname" "pubyear" "subjarea" "language" "af-id" "exactkeyword" "srctype" "country" "aucite")
  "List of facets numbers provided by Scopus.")


(alexandria:define-constant +http-ok+ 200 :test #'=)

(define-bibsystem ("Scopus")
    :accept-encodings '(:json "application/json"
                        :xml+atom "text/xml, application/atom+xml"))



(defmethod rest-endpoint-search ((system <scopus>))
  (declare (ignore system))
  (format nil "~A/~A" (bibsys::config-option system "api-host") *search-endpoint-url*))


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


(defmethod rest-query-parameters ((system <scopus>) query start chunk-size &key format facets)
  (declare (ignore system format facets))
  (list (cons "query" query)
        ;;(cons "fields" fields)
        (cons "facets" (when (zerop start)
                         (format-facets-request *available-facets-names* :count 30)))
        (cons "view" "complete")
        (cons "count" (format nil "~D" chunk-size))
        (cons "start" (format nil "~D" start))))

(defmethod rest-query-headers ((system <scopus>) query &key format facets)
  (declare (ignore system format facets))
  `(("X-ELS-APIKey" . ,(bibsys::config-option system "api-key"))
    ("X-ELS-ResourceVersion" . "XOCS")))


;;; Parse response


(defun get-json-item (json keys-path)
  "Extract value of an item specified by a sequence of json keys."
  (reduce #'(lambda (sub-json key)
              (cdr (assoc key sub-json)))
          (if (listp keys-path) keys-path (list keys-path))
          :initial-value json))

(defun parse-entry (entry-json)
  entry-json)

(defun ensure-multiple-categories (category-data)
  (when category-data
    (if (consp (first category-data))
        (list category-data)
        category-data)))


(defmethod bibsys:parse-response ((system <scopus>) content (format (eql :json)) &key result-object)
  "Parse JSON document returned by Scopus search engine."
  (declare (ignore system))
  (let* ((json (cl-json:decode-json-from-string content))
         (total-results (parse-integer (get-json-item json '(:search-results :opensearch\:total-results))))
         (facets (get-json-item json '(:search-results :facet)))
         (entries (get-json-item json '(:search-results :entry)))
         (result (or result-object (make-instance '<scopus-result> :total-results total-results))))
    ;; Assign facet data
    (when (or (not (slot-boundp result 'bibsys:facets))
              (not (bibsys:facets result)))
      (loop :for name-attribute-category :in facets
         :for name = (cdr (assoc :name name-attribute-category))
         :and category = (ensure-multiple-categories (rest (assoc :category name-attribute-category)))
         :for facet = (make-instance 'bibsys:<facet> :name name)
         :do
         (setf (bibsys:items facet)
               (mapcar (lambda (category-data)
                         (mapcar (lambda (key) (cdr (assoc key category-data)))
                                 '(:name :value :hit-count)))
                        category))
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




(defun request-signature (query format start chunk-size)
  "Compute hash signature of a request to Scopus."
  (flet ((normalize (query)
           (string-upcase query))
         (to-hex (md5-digest)
           (format nil "~(~{~2,'0X~}~)"
                   (map 'list #'identity md5-digest))))
    (to-hex (md5:md5sum-string (format nil "~A:~A:~A:~A" (normalize query) format start chunk-size)))))


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
;;                (setf total-results (parse-integer (get-json-item document '(:search-results :opensearch\:total-results))))))
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
