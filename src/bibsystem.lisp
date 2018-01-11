;;;;
;;;; Unified interface of bibliography systems and queries. The aim is
;;;; to provide a unified interface to query different ssytems, such
;;;; as Scopus, Crossref, or Springr.
;;;;

(in-package :cl-user)

(defpackage saexplorer.bibsystem
  (:nicknames :bibsys)
  (:use :cl :saexplorer.sys :saexplorer.cache)
  (:import-from :cl-log
                #:log-message)
  (:export #:<bibliography-system> #:<query> #:<result> #:<facet>
           #:entries #:items #:facets #:config-option
           #:rest-accept-encoding #:parse-response
           #:define-bibsystem
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
  ((name :type string :initarg :name :initform (error "A name is required!") :accessor system-name)
   (config-options :accessor config-options
                   :documentation "An alist of which the items are
dotted lists of key/value pairs being the option names and values
specified in the config file. By default, options of the System.name
section.")
   (proxy-data :accessor proxy-data)
   (accept-encodings :initarg :accept-encodings
                     :documentation "A plist that maps format names to MIME types used as Accept header for REST requests.")
   (endpoints :accessor endpoints :initarg :endpoints
              :documentation "A list of `optima:match' templates that maps (operation datatype format) triplets to URI strings."))
  (:documentation "Representation of a bibliography system."))


(defclass <identifier> ()
  ((id :type string :accessor id-string
       :documentation "Identifier as a string.")
   (system :type <bibliography-system> :accessor system
           :documentation "The system that issued ID.")
   (aliases :accessor aliases :initform nil
            :documentation "Identifiers of the same document in other systems."))
  (:documentation "Identifier of any entity of a `<bibliography-system>'."))


(defclass <entity> ()
  ((identifier :accessor identifier))
  (:documentation "An identifiable entity in a bibliography
  system. Examples are publication, author, etc."))

;;;
;;; Queries
;;;

(deftype query-operator ()
  "Operator that can be used in a query atomic expressions."
  '(member :equal :starts-with :contains))

;; Allowed field names
(deftype field-name ()
  "Searchable fields of a document. A specific system may not support all the fields."
  '(member
    :aulast ; last name of an author
    :title ; title of the document
    :srctitile ; source titile, e.g. journal or conference proceedings name
    :keywords :authkeywords :description
    :issn :eissn
    :volume :issue
    :pagerange
    :coverdate
    :isbn
    :affiliation
    :doi
    :identifier
    ))

(deftype logical-operation ()
  "Logical operation that may be used to connect parts of a query (atoms or queries)."
  '(member :and :or))


(defun elements-are-of-type (seq type)
  (every #'(lambda (x) (typep x type)) seq))

(deftype list-of-type (type)
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (seq) (elements-are-of-type seq type)))
    `(and list (satisfies ,predicate))))

(defun tripletp (x)
  "Checks that X is a triplet of the form (`query-operator' `field-name' non-nil-value)"
  (and ((not (null (third x)))
        (typep (first x) query-operator)
        (typep (second x) field-name))))

(deftype query-atom ()
  "A constraint on single searchable field."
  '(satisfies tripletp))

(defclass %<query-node> ()
  ((logical-operation :type logical-operation :initform :and)
   (items :type '(list-of-type (or query-atom %<query-node>)) :initform nil))
  (:documentation "Node of a tree corresponding to logical structure of the query."))

(defclass <query> ()
  ((root :type %<query-node>))
  (:documentation "System-independent representation of a search query."))

;;;
;;; Query results
;;;

(defclass <facet> ()
  ((name :type string :accessor name :initarg :name
         :documentation "Name of the facet. This could be the name of
         an attribute appeared in objects returned by the search query.")
   (items :accessor items :initarg :items :initform nil
          :documentation "List of (:name name :value value :count count) triplets."))
  (:documentation "Facet description of a search query. Each facet is
  identified by its name and contains value/name/count triplets, e.g.,
  for facet by country its content is a mapping from
  countryid/countryname into number of documents related to this
  country and matched by the query."))

(defmethod (setf items) :before (new-value (facet <facet>))
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

(defgeneric rest-endpoint (system operation datatype &key format)
  (:documentation "Return SYSTEM's REST interface endpoint URI for OPERATION over DATATYPE.

OPERATION is one of the keywords :SEARCH, or :RETRIEVE.
DATATYPE is the name of dta to be processed, e.g. :AUTHORS, :ARTICLES, or :AFFILIATIONS.
FORMAT is the keyword derived from the format keyword of `query' generic function.
"))

(defgeneric rest-endpoint-search (system)
  (:documentation "Returns endpoint URI for REST search requests."))

(defgeneric rest-query-parameters (system query start chunk-size &key format facets)
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


(defgeneric query (system query &key fields facets format max-results)
  (:documentation "Perform search QUERY on SYSTEM. If FIELDS is given,
  then it is a list of `field-name's to be returned for each
  document."))

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

;;;
;;; Methods
;;;
(alexandria:define-constant +http-ok+ 200 :test #'=)

(defmethod initialize-instance :after ((system <bibliography-system>) &key)
  "Load all items defined in System.name section of the
`*config*'. Compatible with `config-option' method."
  (let ((section-name (concatenate 'string "System." (system-name system))))
    (setf (slot-value system 'config-options)
          (py-configparser:items *config* section-name))
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

(defmethod rest-endpoint ((system <bibliography-system>) operation datatype &key format)
  "Find matching record in system's endpoints slot, if it is bound."
  (when (not (slot-boundp system 'endpoints))
    (error "Endpoints are not defined for system `~A'" (system-name system)))
  (optima:ematch (list operation datatype format) (slot-value system 'endpoints)))


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



(defmethod query ((system <bibliography-system>) query &key fields facets format max-results)
  (declare (ignore fields))
  (let ((endpoint (rest-endpoint-search system))
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
      ;(red:del (signature query (system-name system) format start chunk-size))
      (with-cached-result (chunk-content (signature query (system-name system) format start chunk-size) :timeout timeout-sec)
          (progn
            (log-message :trace "Accessing ~A start=~D, total=~D." (system-name system) start total-results)
            (multiple-value-bind (content status-code)
                (drakma:http-request endpoint
                                     :method :get
                                     :external-format-in :utf-8
                                     :external-format-out :utf-8
                                     ;; :url-encoder #'(lambda (s f)
                                     ;;                 (declare (ignore f))
                                     ;;                 (saexplorer::url-encode s :utf-8))
                                     :proxy
                                     (when (getf (proxy-data system) :host)
                                       (list (getf (proxy-data system) :host)
                                             (or (getf (proxy-data system) :port) 3128)))
                                     :proxy-basic-authorization
                                     (when (getf (proxy-data system) :username)
                                       (list (getf (proxy-data system) :username)
                                             (getf (proxy-data system) :password)))
                                     :additional-headers (rest-query-headers system query :format format :facets facets)
                                     :parameters (rest-query-parameters system query start chunk-size :format format :facets facets)
                                     :accept (rest-accept-encoding system query format))
              (setf request-failed (not (= status-code +http-ok+)))
              (when (= status-code +http-ok+)
                (handler-case
                    (flexi-streams:octets-to-string content :external-format :utf-8)
                  (flexi-streams:external-format-encoding-error (e)
                    ;; Some systems do not encode messages properly?
                    (declare (ignore e))
                    content)))))
        ;;--- should be: (parse-response result chunk-content format)
        (setf result (parse-response system chunk-content format :result-object result))
        ;; extract total results
        (unless total-results
          (setf total-results (total-results result)))))
    result))
