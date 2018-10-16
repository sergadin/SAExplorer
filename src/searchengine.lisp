;;;;
;;;; Generic protocol for querying REST-accessable search engines of any kind.
;;;;

(defpackage :saexplorer.search-engine
  (:nicknames :search-engine)
  (:use :cl :saexplorer.sys)
  (:import-from :cl-log
                #:log-message)
  (:export #:<search-engine> #:<query> #:<document> #:<result>
           #:rest-endpoint #:rest-query-headers #:rest-query-parameters #:rest-accept-encoding
           #:config-option
           #:query))


(defclass <search-engine> (cl-singleton-mixin:singleton-mixin)
  ((name :type string :initarg :name :accessor engine-name)
   (config-options :documentation "An alist of which the items are
key/value pairs being the option names and values specified in the
config file. By default, options of the System.name section.")
   (proxy-data :accessor proxy-data)
   (default-format :initarg :default-format :initform nil
     :documentation "The default format of all documents returned by the system."))
  (:documentation "An engine that supports some sort of 'querying'."))


(defclass <query> ()
  ((allow-degradation-p
    :initarg :allow-degradation
    :accessor allow-degradation-p
    :initform t
    :documentation "If T, which is the default, the query may be
    modified in accordance with searching capabilities provided by a
    search engine it submitted."))
  (:documentation "Superclass of all system-independent queries."))


(defclass <request-context> ()
  (:documentation "Any information describing the current state of the
  query processing, such as the current page and page size for a
  paginated output."))

(defclass <document> ()
  (:documentation "Superclass representing a document returned by a search engine."))


(defclass <result> ()
  ((query :type <query> :initarg :query :reader for-query
          :documentation "The query this result correspondsd to.")
   (engine :reader engine :initarg :engine
                :documentation "An engine the result was received from.")
   (timestamp :accessor timestamp
              :initform (get-universal-time)
              :documentation "Timestamp when the query was processed.")
   (matched-entries :initarg :matched-entries :accessor entries
                    :documentation "A list of records returned by the search engine.")
   (total-results :type (or null (integer 0))
                  :initarg :total-results :accessor total-results
                  :initform nil
                  :documentation "Total number of results found by the search engine."))
  (:documentation "Representation of a query evaluation result."))


;;;
;;; Generic functions
;;;

(defgeneric config-option (engine name &optional default)
  (:documentation "Get value for option NAME."))

(defgeneric rest-endpoint (engine query &key format)
  (:documentation "Return ENGINE's REST interface endpoint URI that
  should be accessed in order to evaluate QUERY with the result
  encoded using FORMAT."))

(defgeneric rest-query-parameters (engine query context &key format)
  (:documentation "Constructs a list of parameters to be passed via
  HTTP GET request. List of conses (name . value)."))

(defgeneric rest-query-headers (engine query context &key format)
  (:documentation "Constructs a list of HTTP headers to be passed via
  REST request in order to process QUERY. List of conses (name . value)."))

(defgeneric rest-query-content (engine query context &key format)
  (:documentation "Constructs a document to be passed as the content
  of a POST request in order to process QUERY."))

(defgeneric rest-accept-encoding (engine query &key format)
  (:documentation "Return MIME type to be passed as an accept-encoding."))

(defgeneric query (search-engine query &key format max-results)
  (:documentation "Evaluate QUERY on SEARCH-ENGINE. MAX-HITS defines
  maximal number of documents to be returned. FORMAT keyword parameter
  specifies encoding format of the output, e.g. :json, :xml, etc."))

(defgeneric make-request-context (engine query &key format)
  (:documentation "Create appropriate `<context>' object."))

(defgeneric make-empty-result (system &key format)
  (:documentation "Make an instance of `<result>' for consuming subsequent chunks of data."))

(defgeneric parse-response (engine query content format &key result request-context)
  (:documentation "Parse a document retured by engine's search
  API. Returns an instance of `<result>'.

RESULT and REQUEST-CONTEXT are updated, if a non-NIL reference is
provided."))

(defgeneric hash-query (query context)
  (:documentation "Compute a hash signature of a QUERY with respect to
  the current CONTEXT. Queries with equal hashes are cached."))

;;;
;;; Default methods implementations
;;;

(defmethod initialize-instance :after ((system <search-engine>) &key)
  "Load all items defined in System.name and Proxy sections of the
`*config*'. Compatible with `config-option' method."
  (let ((section-name (concatenate 'string "System." (system-name system))))
    (setf (slot-value system 'config-options)
          (when (py-configparser:has-section-p *config* section-name)
            (py-configparser:items *config* section-name)))
    (setf (slot-value system 'proxy-data)
          (list :host (py-configparser:get-option *config* "Proxy" "host")
                :port (py-configparser:get-option *config* "Proxy" "port" :type :number)
                :username (py-configparser:get-option *config* "Proxy" "username")
                :password (py-configparser:get-option *config* "Proxy" "password")))))

(defmethod config-option ((engine <engine>) (name string) &optional default)
  (let ((option (assoc name (slot-value engine 'config-options) :test #'string=)))
    (if option
        (string-trim "\"" (cdr option))
        default)))


;;; Query an engine

(defun signature (query &rest args)
  "Compute MD5 hash signature of the QUERY, a system-specific string,
augmented with any number of additional attributes, such as system's
name, or encoding format."
  (flet ((normalize (query)
           (string-upcase query))
         (to-hex (md5-digest)
           (format nil "~(~{~2,'0X~}~)"
                   (map 'list #'identity md5-digest)))
         (mkstr (&rest args)
           (format nil "~{~A~^:~}" args)))
    (to-hex (md5:md5sum-string (mkstr (normalize query) args)))))

(defun serialize (query)
  "Serialize `<query>' instance into a string."
  (typecase query
    (string query) ;--- FIXME: all string queries should be replaced by class instances.
    (t (format nil "~{~A~}" (mapcar #'(lambda (f) (funcall f query))
                                    (list #'query-filters #'query-facets #'query-fields))))))

(defmethod query ((engine <engine>) query &key format max-results)
  "Evaluate search QUERY on ENGINE and return a `<result>' object.

It is assumed that in response to a search query the engine sends a sequence of
matching entries, devided into chunks (pagination). Responses for all requests for
individual chunks, or 'pages', are cached. If the same query with the same paging
parameters (start/offset) is found in cache, no access to external engine is made.

Results are processed by chunks, as far as new data become available. Raw responses
are cached for about a month.

FORMAT specifies requested result formatting. Possible values are ENGINE-dependent,
while typical examples are :JSON, or :XML.

MAX-RESULTS limits maximal number of entries to be acquired from the engine. If NIL,
all results will be downloaded, up to engine's predefined limit, if any.
"
  (let ((endpoint (rest-endpoint engine query :format format))
        (chunk-size 20)
        total-results
        (timeout-sec (* 60 60 24 30)) ;; About one month
        (request-failed nil)
        result)
    (log-message :trace "Querying ~A's endpoint ~A" (engine-name engine) endpoint)
    (do ((start 0 (+ start chunk-size)))
        ((or request-failed
             (and total-results
                  (>= start (min total-results (or max-results total-results))))))
      ;;(red:del (signature (serialize query) (engine-name engine) format start chunk-size))
      (with-cached-result (chunk-content (signature (serialize query) (engine-name engine) format start chunk-size) :timeout timeout-sec)
          (progn
            (log-message :trace "Accessing ~A start=~D, total=~D." (engine-name engine) start total-results)
            (multiple-value-bind (content status-code server-headers)
                (drakma:http-request endpoint
                                     :method :get
                                     :external-format-in :utf-8
                                     :external-format-out :utf-8
                                     :url-encoder #'(lambda (s f)
                                                      (declare (ignore f))
                                                      (saexplorer::url-encode s :utf-8))
                                     :proxy
                                     (when (getf (proxy-data engine) :host)
                                       (list (getf (proxy-data engine) :host)
                                             (or (getf (proxy-data engine) :port) 3128)))
                                     :proxy-basic-authorization
                                     (when (getf (proxy-data engine) :username)
                                       (list (getf (proxy-data engine) :username)
                                             (getf (proxy-data engine) :password)))
                                     :additional-headers (rest-query-headers engine query :format format)
                                     :parameters (rest-query-parameters engine query start chunk-size :format format)
                                     :accept (rest-accept-encoding engine query format))
              (setf request-failed (not (= status-code +http-ok+)))
              (when (= status-code +http-ok+)
                ;; Decode content into string, if needed
                (multiple-value-bind (type subtype params)
                    (drakma:get-content-type server-headers)
                  (if (and (member type '("text") :test #'string-equal)
                           (member subtype '("plain") :test #'string-equal))
                      content
                      (handler-case
                          (progn
                            (log-message :debug "Decoding from content-type: ~A/~A; ~A" type subtype params)
                            (flexi-streams:octets-to-string content :external-format :utf-8))
                        (flexi-streams:external-format-encoding-error (e)
                          ;; Some engines do not encode content properly?
                          (log-message :error "Content decoding failed: ~A" e)
                          content)))))))
        ;; Process cached or just downloaded content
        (if (or (not chunk-content) (string-equal chunk-content "NIL"))
          (setf request-failed t)
          (progn
            ;;--- should be: (parse-response result chunk-content format)
            (setf result (parse-response engine chunk-content format :result-object result))
            ;; extract total results
            (unless total-results
              (setf total-results (total-results result))))))
      #+(or)(red:del (signature query (engine-name engine) format start chunk-size)))
    result))
