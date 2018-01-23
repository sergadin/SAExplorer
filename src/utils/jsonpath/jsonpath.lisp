;;;;
;;;; Visit http://goessner.net/articles/JsonPath/ for the description of JSONPath.
;;;;

(in-package :cl-user)

(defpackage jsonpath
  (:use :cl :jsonpath.parser)
  (:export #:process #:match))

(in-package :jsonpath)

(define-condition jsonpath-error (simple-error)
  ()
  (:documentation "General JSONPath condition."))

(define-condition multiple-values-error (jsonpath-error)
  ()
  (:default-initargs
   :format-control "More than one value returned by an JSONPath filter expression.")
  (:documentation "Signaled when too many values returned by a filtering expression."))


;;;
;;; Processing
;;;

(defun json-lookup (json key)
  (cdr (assoc key json)))

(defun name-to-keyword (name)
  (when (stringp name)
    (intern (string-upcase name) :keyword)))

(defun relation-match-p (relation node document-root)
  "Check that RELATION is satisfied by the NODE of json DOCUMENT-ROOT."
  (destructuring-bind
        (relation-kw relation-str (left-head . left-content) (right-head . right-content))
      relation
    (declare (ignore relation-kw))
    (flet ((operand-value (filter-head filter-content)
             (let ((filter (cons filter-head filter-content)))
               (ecase filter-head
                 (:current (process-parsed node filter))
                 (:root (process-parsed document-root filter))
                 (:constant (car filter-content)))))
           (mkstr (x &optional (force t)) (if force (format nil "~A" x) x))
           (operation-fn (op-name use-string)
             (alexandria:switch (op-name :test #'string-equal)
               ("<" (if use-string #'string-lessp #'<))
               (">" (if use-string #'string-greaterp #'>))
               ("=" (if use-string #'string= #'>))
               ;;--- TODO: add interface for legitimate function registration?
               (t (symbol-function (find-symbol (string-upcase op-name))))))
           (single-value (x)
             (when (and (listp x) (cdr x)) (error 'multiple-values-error))
             (if (consp x) (car x) x)))
      (let* ((left-value (operand-value left-head left-content))
             (right-value (operand-value right-head right-content))
             (use-string (some #'stringp (list left-value right-value))))
        (funcall (operation-fn relation-str use-string)
                 (mkstr (single-value left-value) use-string)
                 (mkstr (single-value right-value) use-string))))))


(defun match-filter-p (node index document-root filter)
  "Check that NODE, an INDEX-th element of his parent's value list, matches FILTER constraint."
  (case (car filter)
    ('nil t) ;; no filter at all
    (:range
     (and index (or (eql (second filter) :any)
                    (destructuring-bind (start end step)
                        (rest filter)
                      (and (<= start index (if (eql end :infinity) index end))
                           (integerp (/ (- index start) step)))))))
    (:predicate (not (null (process-parsed (ecase (caadr filter)
                                             (:current node)
                                             (:root document-root))
                                           (cadr filter)))))
    (:relation (relation-match-p filter node document-root))))

;;
;; (json:decode-json-from-string "[1,2,3]")
;;  ==> (1 2 3)
;; (json:decode-json-from-string "[[[[1,2]]]]")
;;  ==> ((((1 2))))
;; (json:decode-json-from-string "{\"a\": 1, \"b": 2}")
;;   ==> ((:A . 1) (:B . 2))
;; (json:decode-json-from-string "{\"a\": {\"b\": 1}}")
;;  ==> ((:A (:B . 1)))
;; (json:decode-json-from-string "{\"a\": {\"b\": 1, \"c\": 2}}")
;;  ==> ((:A (:B . 1) (:C . 2)))
;; (json:decode-json-from-string "{\"a\": [{\"b\": 1}, {\"c\": 2}]}")
;;  ==> ((:A ((:B . 1)) ((:C . 2))))
;; (json:decode-json-from-string "[{\"b\": 1}, {\"c\": 2}]")
;;  ==> (((:B . 1)) ((:C . 2)))

(defun node-type (json)
  (cond
    ;; k: constant
    ((atom json) :atom)
    ;; k: [1, 2, 3]
    ((and (listp json) (atom (car json))) :list)
    ;; k: [{...}, {...}]
    ((and (consp (car json)) (consp (caar json))) :list)
    ;; k: {...}
    ((and (consp (car json)) (keywordp (caar json))) :dict)
    (t nil)))

(defun node-value (json)
  (case (node-type json)
    (:dict (cdar json))
    (:list (cdr json))
    (t json)))


(defun process-parsed (document-root parsed-expression)
  "Evaluate parsed JSONPath expression over json document DOCUMENT-ROOT."
  (let ((matched-nodes nil))
    (labels
        ((scan-list (values filter next-expr-items)
           ;; Call %process for all items of VALUES that match FILTER
           (loop for value-list-item in values and index from 0
              when (or (not filter)
                       (match-filter-p value-list-item index document-root filter))
              do (%process value-list-item next-expr-items)))
         (%process (current expr-items)
           ;; Walk on json tree rooted at CURRENT. Matching nodes are pushed into
           ;; MATCHED-NODES. A node is matched if expr-items is empty.
           (when (null expr-items)
             (pushnew (node-value current) matched-nodes))
           (when (or (null expr-items) (null current))
             (return-from %process nil))
           (destructuring-bind (descend node-name filter)
               (first expr-items)
             (let ((wildcardp (eql node-name :any))
                   (node-id (name-to-keyword node-name)))
               (case (node-type current)
                 (:dict
                  (loop for (key . value) in current do
                       (when (or (eql key node-id) wildcardp)
                         (ecase (node-type value)
                           (:list (scan-list value filter (rest expr-items)))
                           ((:atom :dict)
                            (when (match-filter-p value nil document-root filter)
                              (%process value (rest expr-items))))))
                       (when (eql descend :deep)
                         (%process value expr-items))))
                 (:list
                  (when wildcardp (scan-list current filter (rest expr-items)))
                  (when (eql descend :deep)
                    (scan-list current nil expr-items))))))))
      (%process document-root (cdr parsed-expression))
      (nreverse matched-nodes))))


(defun process (json expression)
  "Evaluate JSONPath expression EXPRESSION over JSON."
  (let ((parsed-expression (parse expression)))
    (process-parsed json parsed-expression)))

(defun match (json expression)
  "Evaluate JSONPath expression EXPRESSION over JSON and return first match."
  (first (process json expression)))
