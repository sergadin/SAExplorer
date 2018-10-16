;;;;
;;;; Unified interface of bibliography systems and queries. The aim is
;;;; to provide a unified interface to query different ssytems, such
;;;; as Scopus, Crossref, or Springr.
;;;;
;;;; Main concepts are: bibliography system, document, query, and result.
;;;;

(in-package :cl-user)

(defpackage saexplorer.bibsystem
  (:nicknames :bibsys)
  (:use :cl :saexplorer.sys :saexplorer.cache)
  (:import-from :cl-log
                #:log-message)
  (:export #:<bibliography-system>
           #:<query> #:<search-query> #:<publ-search-query> #:<author-search-query> #:<conf-search-query>
           #:<publication-document> #:make-identifier
           #:make-jsonpath-getter
           #:query-filters
           #:<result> #:<facet> #:<document>
           #:entries #:facet-items #:facets #:config-option
           #:get-facet
           #:rest-accept-encoding #:parse-response
           #:define-bibsystem #:find-system
           #:retrieve
           #:make-query))

(in-package :saexplorer.bibsystem)

(defvar *registered-systems* nil
  "An alist that maps systems names to instances of `<bibliography-system>'.")

(defun find-system (name)
  "Return an instance of `<bibliography-system>' class associated to the NAME."
  (let ((name-keyword (if (keywordp name)
                          name
                          (intern (string-upcase name) :keyword))))
    (make-instance (cdr (assoc name-keyword *registered-systems*)))))


(defclass <bibliography-system> (cl-singleton-mixin:singleton-mixin)
  ((name :type string
         :initarg :name :initform (error "A name is required!")
         :accessor system-name)
   (config-options :accessor config-options
                   :documentation "An alist of which the items are dotted lists of
key/value pairs being the option names and values specified in the config file. By
default, options of the System.name section.")
   (proxy-data :accessor proxy-data)
   (accept-encodings :initarg :accept-encodings
                     :documentation "A plist that maps format names to MIME types used
                     as Accept header for REST requests.")
   (default-format :initarg :default-format
     :documentation "")
   (endpoints :accessor endpoints :initarg :endpoints
              :documentation "A list of `optima:match' templates that maps (operation
              datatype format) triplets to URI strings."))
  (:documentation "Representation of a bibliography system."))


;;;
;;; Documents
;;;

(defclass <identifier> ()
  ((id :initarg :id-string
       :type string :accessor id-string
       :documentation "Identifier as a string.")
   (system :initarg :system
           :type <bibliography-system> :accessor id-system
           :documentation "The system that issued ID.")
   (aliases :initarg :aliases :initform '()
            :accessor id-aliases
            :documentation "Identifiers of the same document in other systems."))
  (:documentation "Identifier of any entity of a `<bibliography-system>'."))

(defun make-identifier (id-string system &optional aliases)
  (declare (ignore aliases))
  (let ((the-system (etypecase system
                      (string (find-system system))
                      (<bibliography-system> system))))
    (make-instance '<identifier> :id-string id-string :system the-system)))

(defclass <document> ()
  ((identifier :initarg :identifier :accessor identifier)
   (source :initarg :source-system
           :documentation "The `<bibliography-system>' the document was acquired.")
   (content :initarg :content
            :reader document-content
            :documentation "Original content obtained from the system.")
   (format :initarg :format :initform (error "Format is required")
           :documentation "Format of the document's CONTENT.")
   (retrievedp :initarg :retrievedp :initform nil
               :documentation "Was this document retrieved, or just
               extracted from the response to a search request?")
   (getters :reader document-getters
            :initarg :getters
            :documentation "A mapping from property name to property
            getters. A getter is a function with single argument, the
            content of the document.")
   (valid-properties :initarg :valid-properties
                     :allocation :class
                     :reader document-valid-properties
                     :documentation "A list of supported attributes."))
  (:documentation "An identifiable entity in a bibliography system, for example
  publication, or author."))

(defgeneric document-property (document property &optional default)
  (:documentation "Extract PROPERTY value from the DOCUMENT."))


(defmethod document-property ((document <document>) property &optional (default nil supplied-p-default))
  "Extract document's property using its getters."
  ;;--- TODO: signal an error when no getter exists.
  (let* ((the-getter (getf (document-getters document) property))
         (value (when the-getter
                  (funcall the-getter
                           (document-content document)))))
    (if (and (null value) supplied-p-default)
        default
        value)))

(fare-memoization:define-memo-function make-jsonpath-getter (getters-def)
  "Make getters from the supplied definitions GETTERS-DEF. Getters definiton is an
alist that maps property names to jsdonpath expressions."
  (loop for (key . expr) in getters-def
     nconcing (list key (let ((access-path expr)) ; catch expr value
                          #'(lambda (json)
                              (jsonpath:match json access-path))))))


;;;
;;; Queries
;;;

(defclass <query> ()
  ((allow-degradation-p
    :initarg :allow-degradation
    :accessor allow-degradation-p
    :initform t
    :documentation "If T, which is the default, the query may be modified in
    accordance with searching capabilities provided by specific system."))
  (:documentation "Superclass of all system-independent queries."))

(defclass <search-query> (<query>)
  ((filters :initform nil
            :accessor query-filters
            :documentation "The query itself. Defines conditions that should be
            fulfilled in oreder to include an entity into result. In its simplest
            form, it is just a keyword that should appear somewere in the item.")
   (facets :initarg :facets
           :initform nil
           :accessor query-facets
           :documentation "List of requested facets. Each facet is either a facet
           name, or a pair (:facet-name depth), where depth is a number of items to be
           computed.")
   (fields :initarg :fields
           :initform nil
           :accessor query-fields
           :documentation "List of properties to be returned for each matched item."))
  (:documentation "Superclass of all searching queries."))

(defclass <retrieve-query> (<query>)
  ((identifier :type 'string
               :initarg :identifier
               :accessor query-identifier))
  (:documentation "Superclass of all object retrieval queries."))


;;;
;;; Query results
;;;

(defclass <facet> ()
  ((name :type string :accessor name :initarg :name
         :documentation "Name of the facet. This could be the name of an attribute
         appeared in objects returned by the search query.")
   (items :accessor facet-items :initarg :items :initform nil
          :documentation "List of (:name name :value value :count count) triplets."))
  (:documentation "Facet description of a search query. Each facet is identified by
  its name and contains value/name/count triplets, e.g., for facet by country its
  content is a mapping from countryid/countryname into number of documents related to
  this country and matched by the query."))

(defmethod (setf facet-items) :before (new-value (facet <facet>))
  "Verify that NEW-VALUE of FACET's content is a plist."
  (mapc #'(lambda (item)
            (assert (rutils:plistp item) (item)
                    "Invalid facet items structure; plist expected: ~S" item))
        new-value))


(defclass <result> ()
  ((query :type <query> :initarg :query :reader for-query
          :documentation "The query this result correspondsd to.")
   (system :type <bibliography-system> :reader system :initarg :system
           :documentation "The system from which the result was received.")
   (timestamp :accessor timestamp
              :initform (get-universal-time)
              :documentation "Timestamp when the query was processed.")
   (matched-entries :initarg :matched-entries :accessor entries
                    :documentation "A list of records returned by the search engine.")
   (total-results :initarg :total-results :accessor total-results
                  :documentation "Total number of results found by search engine.")
   (facets :accessor facets :initarg :facets
           :documentation "A list of `<facet>' describing query result."))
  (:documentation "Representation of a query evaluation result."))

;;;
;;; Utility macros
;;;
(defmacro define-bibsystem ((name &optional class-name) &key accept-encodings rest-endpoints json-result-mapping)
  "Generete definition of system's classes and related stuff.
"
  (let* ((name-str (format nil "~A" name))
         (name-upstr (string-upcase (format nil "~A" (or class-name name))))
         (system-class-name (intern (format nil "<~A>" name-upstr)))
         (system-key (intern name-upstr :keyword))
         (result-class-name (intern (format nil "<~A-RESULT>" name-upstr))))
  `(progn
     ;; Create system class
     (defclass ,system-class-name (saexplorer.bibsystem::<bibliography-system>)
       ()
       (:default-initargs
        :name ,name-str
         ,@(when accept-encodings `(:accept-encodings ,accept-encodings))))
     ;; Register system
     (when (null (assoc ,system-key saexplorer.bibsystem::*registered-systems*))
       (push (cons ,system-key ',system-class-name) saexplorer.bibsystem::*registered-systems*))
     ;; Create system's result class
     (defclass ,result-class-name (saexplorer.bibsystem::<result>)
       ()
       (:default-initargs :matched-entries nil :facets nil))
     t)))

;;;
;;; Generic functions
;;;

(defgeneric rest-endpoint (system query &key format)
  (:documentation "Return SYSTEM's REST interface endpoint URI that
  should be accessed in order to evaluate the QUERY with the result
  encoded using FORMAT.

FORMAT is the keyword derived from the format keyword of `query' generic function.
"))

(defgeneric rest-query-parameters (system query start chunk-size &key format)
  (:documentation "Constructs a list of parameters to be passed via
  REST request. List of conses (name . value)."))

(defgeneric rest-accept-encoding (system query format)
  (:documentation "Return MIME type to be passed as an accept-encoding."))

(defgeneric rest-query-headers (system query &key format facets)
  (:documentation "Constructs a list of HTTP headers to be passed via
  REST request. List of conses (name . value).")
  (:method ((system <bibliography-system>) query &key format facets)
    (declare (ignore system query format facets))
    nil))

(defgeneric config-option (system name &optional default)
  (:documentation "Get value for option NAME.")
  (:method ((system <bibliography-system>) (name string) &optional default)
    (let ((option (assoc name (slot-value system 'config-options) :test #'string=)))
      (if option
          (string-trim "\"" (cdr option))
          default))))


(defgeneric query (system query &key format max-results)
  (:documentation "Perform search QUERY on SYSTEM."))

(defgeneric make-empty-result (system &key format)
  (:documentation "Make an instance of `<result>' for consuming subsequent chunks of data."))

(defgeneric parse-response (system content format &key result-object)
  (:documentation "Parse a document retured by system's search
  API. Returns an instance of `<result>'.

If RESULT-OBJECT is non-NIL, then this as an instance of `<result>' to
be updated.
"))


(defgeneric retrieve (system document-id fields)
  (:documentation "Retrieve from the SYSTEM all data specified by
  FIELDS about object identified by DOCUMENT-ID."))

(defgeneric get-facet (result facet-name &key default)
  (:documentation "Find a facet by name.")
  (:method ((result <result>) (facet-name string) &key (default nil default-bound-p))
    (let ((facet (find facet-name (facets result) :key #'name :test #'string-equal)))
      (if (or facet (not (null default-bound-p)))
          (or facet default)
          (error "Facet `~A' NOT FOUND in `~A'." facet-name result)))))


;;;
;;; Methods
;;;
(alexandria:define-constant +http-ok+ 200 :test #'=)

(defmethod initialize-instance :after ((system <bibliography-system>) &key)
  "Load all items defined in System.name section of the
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

(defmethod rest-accept-encoding ((system <bibliography-system>) query format)
  "Determine acceptable HTTP encoding for a query in given FORMAT by
searching FORMAT in a plist of `accept-encoding' slot, if bound."
  (declare (ignore query))
  (if (slot-boundp system 'accept-encodings)
      (getf (slot-value system 'accept-encodings) format nil)
      "text/xml, application/json"))

(defmethod rest-endpoint ((system <bibliography-system>) (query <query>) &key format)
  "Find matching record in system's endpoints slot, if it is bound."
  (when (not (slot-boundp system 'endpoints))
    (error "Endpoints are not defined for system `~A'" (system-name system)))
  (optima:ematch (list operation datatype format) (slot-value system 'endpoints)))


;;; Pretty printing

(defmethod print-object ((result <result>) out)
  (print-unreadable-object (result out :type t)
    (format out "~s out of ~s results." (length (entries result)) (total-results result))))

;;; Querying bibsystem

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

(defmethod query ((system <bibliography-system>) query &key format max-results)
  "Evaluate search QUERY on SYSTEM and return a `<result>' object.

It is assumed that in response to a search query the system sends a sequence of
matching entries, devided into chunks (pagination). Responses for all requests for
individual chunks, or 'pages', are cached. If the same query with the same paging
parameters (start/offset) is found in cache, no access to external system is made.

Results are processed by chunks, as far as new data become available. Raw responses
are cached for about a month.

FORMAT specifies requested result formatting. Possible values are SYSTEM-dependent,
while typical examples are :JSON, or :XML.

MAX-RESULTS limits maximal number of entries to be acquired from the system. If NIL,
all results will be downloaded, up to system's predefined limit, if any.
"
  (let ((endpoint (rest-endpoint system query :format format))
        (chunk-size 20)
        total-results
        (timeout-sec (* 60 60 24 30)) ;; About one month
        (request-failed nil)
        result)
    (log-message :trace "Querying ~A's endpoint ~A" (system-name system) endpoint)
    (do ((start 0 (+ start chunk-size)))
        ((or request-failed
             (and total-results
                  (>= start (min total-results (or max-results total-results))))))
      ;;(red:del (signature (serialize query) (system-name system) format start chunk-size))
      (with-cached-result (chunk-content (signature (serialize query) (system-name system) format start chunk-size) :timeout timeout-sec)
          (progn
            (log-message :trace "Accessing ~A start=~D, total=~D." (system-name system) start total-results)
            (multiple-value-bind (content status-code server-headers)
                (drakma:http-request endpoint
                                     :method :get
                                     :external-format-in :utf-8
                                     :external-format-out :utf-8
                                     :url-encoder #'(lambda (s f)
                                                      (declare (ignore f))
                                                      (saexplorer::url-encode s :utf-8))
                                     :proxy
                                     (when (getf (proxy-data system) :host)
                                       (list (getf (proxy-data system) :host)
                                             (or (getf (proxy-data system) :port) 3128)))
                                     :proxy-basic-authorization
                                     (when (getf (proxy-data system) :username)
                                       (list (getf (proxy-data system) :username)
                                             (getf (proxy-data system) :password)))
                                     :additional-headers (rest-query-headers system query :format format)
                                     :parameters (rest-query-parameters system query start chunk-size :format format)
                                     :accept (rest-accept-encoding system query format))
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
                          ;; Some systems do not encode messages properly?
                          (log-message :error "Content decoding failed: ~A" e)
                          content)))))))
        ;; Process cached or just downloaded content
        (if (or (not chunk-content) (string-equal chunk-content "NIL"))
          (setf request-failed t)
          (progn
            ;;--- should be: (parse-response result chunk-content format)
            (setf result (parse-response system chunk-content format :result-object result))
            ;; extract total results
            (unless total-results
              (setf total-results (total-results result))))))
      #+(or)(red:del (signature query (system-name system) format start chunk-size)))
    result))
