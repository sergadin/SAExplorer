;;;; Query  builder

(in-package :saexplorer.bibsystem)

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
    :term ; anyware
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



(defun make-query-node (op keyword value)
  (let ((qn (make-instance '%<query-node>)))
    (with-slots ((items items))
        qn
      (push (list op keyword value) items))
    qn))

(defun push-root-node (the-query op keyword value)
  (with-slots ((root root))
      the-query
  (if (not (slot-boundp the-query 'root))
      (setf root (make-query-node op keyword value))
      (push (list op keyword value) (slot-value root 'items)))))



(defclass <publ-search-query> (<search-query>)
  ()
  (:documentation "Publications search query."))

(defclass <conf-search-query> (<search-query>)
  ()
  (:documentation "Conference search query."))

(defclass <author-search-query> (<search-query>)
  ()
  (:documentation "Author search query."))


(defgeneric make-simple-query (query term)
  (:method ((query <search-query>) (term string))
    (setf (query-filters query) `(:equal :term ,term))
    query))



(defmacro make-query (&rest clauses)
  "Creates a `<query>' object from specified CLAUSES. Each clause is
 either an expression of the form (operator :field value), or (and/or
 clauses) expression.

 Example:
 (query (and (in \"a topic\" :keywords) (= :aulast \"Ivanov\")))
"
  (let ((the-query (gensym)))
    `(let ((,the-query (make-instance '<query>)))
       ,@(loop
            :with code = '()
            :for statement :in clauses
            :do
            (alexandria:eswitch ((string-upcase (first statement)) :test #'string=)
              ("=" (destructuring-bind (op fieldname value)
                       statement
                     (declare (ignore op))
                     (push `(push-root-node ,the-query :equal ,fieldname ,value) code))))
            :finally (return (reverse code)))
       ,the-query)))
