
(in-package :cl-user)

(defpackage saexplorer.crossref
  (:nicknames :crossref)
  (:use :cl :saexplorer.bibsystem)
  (:import-from :saexplorer
                #:*dbcon*)
  (:import-from :cl-log
                #:log-message))

(in-package :saexplorer.crossref)

(define-bibsystem ("Crossref")
    :accept-encodings '(:json "application/json"))


(defparameter *crossref-api-host* "https://api.crossref.org")
(defparameter *search-endpoint-url* "works" "URL, without the leading '/', to submit search queries.")

;; Do not use proxy, as it requires authentication on the AMiner site.
(defmethod bibsys::proxy-data ((system <crossref>))
  (declare (ignore system))
  nil)


(defmethod bibsys::rest-endpoint ((system <crossref>) query &key format)
  (declare (ignore system query format))
  (format nil "~A/~A" *crossref-api-host* *search-endpoint-url*)
  "https://api.crossref.org/works")


(defmethod bibsys::rest-query-parameters ((system <crossref>) query start chunk-size &key format)
  (declare (ignore system format))
  (list (cons "query" query)
        (cons "rows" (format nil "~D" chunk-size))
        (cons "offset" (format nil "~D" start))
        (cons "mailto" "serg@msu.ru")))

;;;
;;; Fetch works
;;;

(defun parse-json-result (octets)
  (let ((response
         (handler-bind ((json:no-char-for-code
                         #'(lambda (c)
                             (declare (ignore c))
                             (invoke-restart 'json:substitute-char #\?))))
           (json:decode-json-from-string
            (flexi-streams:octets-to-string octets :external-format :utf-8)))))
    response))
;;  (let* ((response (json:decode-json-from-string
;;                    (html-entities:decode-entities
;;                     (flexi-streams:octets-to-string content :external-format :utf-8))))
;;         (data (cdr (assoc :message response))))


(defun fetch-works (&key (cursor "*"))
  (multiple-value-bind (content status)
      (drakma:http-request "https://api.crossref.org/types/proceedings-article/works"
                           :accept "application/json"
                           :parameters (list (cons "mailto" "serg@msu.ru")
                                             (cons "rows" "10")
                                             (cons "cursor" cursor)))
    (when (/= status 200)
      (error 'network-error :code status))
    (let* ((response (parse-json-result content))
           (data (cdr (assoc :message response))))
      ;;(format t " FW: ~A~%     ~A~%" cursor (cdr (assoc :next-cursor data)))
      data
      (flexi-streams:octets-to-string content :external-format :utf-8))))

(defun fetch-all-proceedings ()
  (loop
     :for cursor = "*" :then "*" ;(cdr (assoc :next-cursor data))
     :for data = (fetch-works :cursor cursor)
     :for k :from 0
     :while (and (< k 1) data)
     :do
       (format t "~A: ~A~%" k cursor)
       (with-open-file (f (format nil "f:\\crossref-new\\original-proc-~5,'0D.json" k)
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :direction :output)
         (format f "~A" data)
         (let ((result (jsown:parse data)))
           ;;(format t "~A~%" result)
           ;;(format t "~A~%" (assoc "message" (rest result) :test #'string=))
           (format f "~&~A" (jsown:to-json (cdr (assoc "message" (rest result) :test #'string=)))))
         #+(or)(json:encode-json data f))))


(defun %date-parts-to-date (node)
  "Converts crossref's date-parts field into date."
  (when (= 3 (length (cadar node)))
    (destructuring-bind (year month day)
        (cadar node)
      (format nil "~A-~A-~A" year month day))))

(defun %parse-proceeding-article-record (item)
  "Returns a tab separated string."
  (let ((title (jsonpath:match item "$.title[0]"))
        (authors (format nil "~{~A~^; ~}"
                         (loop for au in (jsonpath:match item "$.author")
                            collect (format nil "~A~@[, ~A~]"
                                            (jsonpath:match au "$.family")
                                            (jsonpath:match au "$.given")))))
        (doi (jsonpath:match item "$.+DOI+"))
        (type (jsonpath:match item "$.type"))
        (deposited (%date-parts-to-date (jsonpath:match item "$.deposited")))
        (event-name (jsonpath:match item "$.event.name"))
        (event-start (%date-parts-to-date (jsonpath:match item "$.event.start")))
        (event-end (%date-parts-to-date (jsonpath:match item "$.event.end")))
        (publisher (jsonpath:match item "$.publisher")))
    (format nil (format nil "~~{~~A~~^~C~~}" #\Tab)
            (mapcar #'(lambda (s) (substitute #\Space #\Tab s))
                    (list type title authors doi deposited event-name event-start event-end publisher)))))

(defun process-proceedings ()
  (let* ((na-sql (concatenate 'string
                              "INSERT INTO article ("
                              "f_article_type"
                              ", f_article_title"
                              ", f_article_authors"
                              ", f_article_doi"
                              ", f_article_deposited"
                              ", f_article_eventname"
                              ", f_article_eventstart"
                              ", f_article_eventend"
                              ", f_article_publisher) "
                              "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"))
         (new-article-query (dbi:prepare *dbcon* na-sql)))
    (with-open-file (out "f:\\crossref\\all-conference-papers.tsv" :direction :output)
      (dotimes (k 5654)
        (log-message :trace "Processing file number ~A." k)
        (with-open-file (f (format nil "f:\\crossref\\proc-~5,'0D.json" k)
                           :direction :input)
          (do* ((data (json:decode-json f))
                (items (cdr (assoc :items data)) (cdr items))
                (item (car items) (car items)))
               ((null items))
            (handler-case
                (format out "~A~%" (%parse-proceeding-article-record item))
              (T ()
                (log-message :error "Unable to parse article record"))
              #+(or)(dbi:execute new-article-query
                      type title authors doi deposited event-name event-start event-end publisher))))))))


(defun load-names ()
  (with-open-file (f "f:\\crossref\\all-conference-papers.tsv")
    (loop
       with hist = (make-hash-table :test #'equal)
       for line = (read-line f nil nil)
       for seq from 0
       for line-items = (split-sequence:split-sequence #\Tab line)
       while line
       do
         (if (<= 9 (length line-items))
             (incf (gethash (elt line-items 8) hist 0))
             (print line-items))
         (when (< 10000 seq)
           (loop-finish))
       finally
         (loop for publisher being the hash-keys of hist
              do (print (list publisher (gethash publisher hist)))))))


;;;
;;; Parsing response
;;;

(defparameter *json-publication-getters*
  `((:id . "$.doi")
    (:title . "$.title[0]")
    (:abstract . nil)
    (:authors . "$.author")
    (:keywords . "$.keywords")
    (:lang . nil)
    (:start-page . "$.page")
    (:end-page . "$.page")
    (:year . "$.issued.date-parts[0]")
    (:venue-name . "$.container-title[0]")
    (:doi . "$.+doi+")
    (:fulltext-url . "$.link[?(@.intendent-application='text-mining')]"))
  "Map attributes names to JSONPath expressions used to extract the
  given attribute from parsed publication.")


(defun parse-entry-json (entry-json)
  (let ((document-id (jsonpath:match entry-json "$.doi"))
        (system (find-system "crossref")))
    (make-instance '<publication-document>
                   :identifier (make-identifier "document-id" system)
                   :source-system system
                   :content entry-json :format :json
                   :getters (make-jsonpath-getter *json-publication-getters*))))


(defmethod bibsys::parse-response ((system <crossref>) content format &key result-object)
  (declare (ignore system format))
  (let* ((document (cl-json:decode-json-from-string content))
         (total-results (jsonpath:match document "$.message.total-results"))
         (entries (jsonpath:match document "$.message.items"))
         (result (or result-object (make-instance '<crossref-result> :total-results total-results))))
    (setf (bibsys:entries result) (append (bibsys:entries result)
                                          (mapcar #'parse-entry-json entries)))
    result))



(defun query-crossref-openurl (&key (query "+afonin +regular"))
  "year - 2013, type in ('Journal Article', 'Chapter', 'Conference Paper', ...)"
  (let* ((coins-document
          (drakma:http-request "http://search.crossref.org/"
                               :parameters (list (cons "q" query)
                                                 (cons "type" "Journal Article")
                                                 (cons "page" "1")
                                                 (cons "rows" "50")
                                                 (cons "sort" "year")
                                                 (cons "header" "true")
                                                 (cons "format" "json"))))
         (document (chtml:parse coins-document (cxml-stp:make-builder))))
    (stp:do-recursively (span document)
      (when (and (typep span 'stp:element)
                 (equal (stp:local-name span) "span")
                 (equal (stp:attribute-value span "class") "Z3988"))
        (format t "~A~%~%"
                (parse-openurl (stp:attribute-value span "title")))))))



(defun query-crossref (query &key (page "1") (rows "3") (sort-by "relevance"))
  "year - 2013, type in {'Journal Article', 'Chapter', 'Conference Paper', ...}.
sort-by in {'relevance', 'year'}
"
  (multiple-value-bind (content status)
      (drakma:http-request "http://search.crossref.org/dois"
                           :parameters (list (cons "q" query)
                                             (cons "header" "true")
                                             ;;(cons "type" "Journal Article")
                                             (cons "page" page)
                                             (cons "rows" rows)
                                             (cons "sort" sort-by)))
    (when (/= status 200)
      (error 'network-error :code status))
    (let* ((response (json:decode-json-from-string
                      (html-entities:decode-entities
                       (flexi-streams:octets-to-string content :external-format :utf-8))))
           (data (cdr (assoc :items response))))
      data)))


(defun crossref-lookup-citations (articles)
  "Поиск публикации в CrossRef по её текстовой ссылке."
  (let ((encoded-articles
         (json:encode-json-to-string
          (if (stringp articles)
              (list articles)
              articles))))
    (multiple-value-bind (content status)
        (drakma:http-request "http://search.crossref.org/links"
                             :accept "application/json"
                             :method :POST
                             :content-type "application/json"
                             :content encoded-articles
                             :external-format-in :utf-8
                             :external-format-out :utf-8)
      (when (/= status 200)
        (error 'network-error :code status))
      (print (flexi-streams:octets-to-string content :external-format :utf-8))
      (let ((crossref-response
             (json:decode-json-from-string
              (dehtml-string
               (flexi-streams:octets-to-string content :external-format :utf-8)))))
        (mapcar #'(lambda (citation)
                    (parse-coins (cdr (assoc :coins citation))))
                (cdr (assoc :results crossref-response)))))))


(defun doi->info (doi style)
  (let ((accept (ecase style
                  (:bibtex "text/bibliography; style=bibtex")
                  (:json "application/citeproc+json"))))
    (multiple-value-bind (content status)
        (drakma:http-request (format nil "http://dx.doi.org/~A" doi)
                             :accept accept
                             :external-format-out :utf-8
                             :external-format-in :utf-8)
      (when (= status 200)
        (if (or (stringp content) (null content))
            content
            "Unable to decode server response from UTF-8")))))

(defun doi->bibtex (doi)
  (doi->info (or doi "10.1142/S0129054106003954") :bibtex))

(defun doi->json (doi &key (decode t))
  (let ((data (doi->info (or doi "10.1142/S0129054106003954") :json)))
    (if (and decode data)
        (json:decode-json-from-string data)
        data)))


(defun doi->meta (doi)
  (let ((accept "application/json"))
    (multiple-value-bind (content status)
        (drakma:http-request (format nil "http://api.crossref.org/works/~A" doi)
                             :accept accept
                             :external-format-in :utf-8)
      (when (= status 200)
        (flexi-streams:octets-to-string content :external-format :utf-8)))))



(defun get-conferences (year month)
  (let ((accept "application/json")
        (cursor "*")
        (filter "from-update-date:2018-12-01,until-update-date:2018-12-01,type:proceedings-article"))
    (multiple-value-bind (content status)
        (drakma:http-request (format nil "~A/works/"
                                     *crossref-api-host*)
                             :parameters (list
                                          (cons "filter" filter)
                                          (cons "mailto" "serg@msu.ru")
                                          (cons "cursor" cursor))
                             :accept accept
                             :external-format-in :utf-8)
      (when (= status 200)
        (let* ((data (flexi-streams:octets-to-string content :external-format :utf-8))
               (js-doc (json:decode-json-from-string data)))
          (let ((total-results (jsonpath:match js-doc "$.message.total-results"))
                (next-cursor (jsonpath:match js-doc "$.message.next-cursor")))
            next-cursor))))))
