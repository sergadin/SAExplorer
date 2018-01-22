;;;;
;;;; Visit http://goessner.net/articles/JsonPath/ for the description of JSONPath.
;;;;

(in-package :cl-user)

(defpackage jsonpath.parser
  (:use :cl :esrap)
  (:shadow :parse)
  (:export #:parse))

(in-package :jsonpath.parser)

;;; JSONPath grammar

(defrule jsonpath
    (and (or root current)
         (* (and successor-mark jsonpath-item)))
  (:destructure (root marks-and-items)
    ;; Convert items to a list of triplets
    (cons root (mapcar #'(lambda (mi)
                           (cons (car mi) (cadr mi)))
                       marks-and-items))))

(defrule root "$" (:constant :root))
(defrule current "@" (:constant :current))

(defrule successor-mark
    (or direct-successor deep-successor))

(defrule direct-successor
    (and "." (! "."))
  (:constant :direct))

(defrule deep-successor
    ".."
  (:constant :deep))

;;
;; item
;;
(defrule jsonpath-item
    (and (or symbol "*")
         (? jsonpath-item-filter))
  (:destructure (key filter)
    (list (if (string= key "*") :any key) filter)))

(defrule jsonpath-item-filter
    (and "[" filter-expr "]")
  (:destructure (br1 expr br2)
    expr))

(defrule filter-expr
    (or filter-expr-wildcard
        filter-index-range
        filter-index-range-defaulted
        filter-predicate-expression)
  (:identity t))

(defrule filter-expr-wildcard "*" (:constant '(:range :any)))

(defrule filter-index-range
    (or (and integer (! ":"))
        (and integer ":" integer (! ":"))
        (and integer ":" integer ":" integer))
  (:lambda (range)
    (let ((start (nth 0 range))
          (end (nth 2 range))
          (step (nth 4 range)))
    (list :range start (or end start) (or step 1)))))

(defrule filter-index-range-defaulted
    (or (and integer ":" (! integer))
        (and ":" integer (! ":")))
  (:lambda (range)
    (if (integerp (first range))
        `(:range ,(first range) :infinity 1)
        `(:range 0 ,(nth 1 range) 1))))


;; Examples:
;;   [?(@.issn)]
;;   [?(@.price < 10)]
;;   [?(@.price > $.x.y.z)]
(defrule filter-predicate-expression
    (and "?" "(" (or predicate relation) ")")
  (:lambda (x) (nth 2 x)))

(defrule predicate
    (and jsonpath (& ")"))
  (:lambda (p)
    ;; drop closing bracket
    (cons :predicate (list (first p)))))

(defrule relation
    (and jsonpath
         (* " ")
         (or "<" ">" "=" symbol)
         (* " ")
         (or jsonpath constant))
  (:destructure (left sp1 operation sp2 right)
    (declare (ignore sp1 sp2))
    (list :relation operation left right)))

(defrule constant
    (or integer string)
  (:lambda (const)
    `(:constant ,const)))

(defrule operator
    (or "<" ">" "=")
  (:identity t))

;;;
;;; Some common parsing rules: numbers, strings, etc.
;;;
(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-apostrophe (char)
  (not (eql #\' char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defrule symbol (and alphanumeric (* (or alphanumeric #\- #\_ #\: #\@)))
  (:lambda (list)
    (text list)))

(defrule alphanumeric (alphanumericp character))

(defrule whitespace (and (+ (or #\space #\tab #\newline)) (? (and comment (? whitespace))))
  (:constant nil))

(defrule integer (and (? (or "+" "-")) (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule string (or simple-string simple-apostrophe-string))

(defrule simple-string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule simple-apostrophe-string (and #\' (* apostrophe-string-char) #\')
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))


(defrule string-char (or (not-doublequote character) (and #\\ #\")))

(defrule apostrophe-string-char (or (not-apostrophe character) (and #\\ #\')))

;;;
;;; Interface function
;;;
(defun parse (jsonpath-expression)
  "Parse jsonpath expression.

Parsed form of the expression as a list. First element is either :ROOT, if the
eqpression should be evaluated from the very top of the document, or :CURRENT. The rest
elements are definitions of path expression items. Each item is a triplet of the
form (DESCEND-RELATION NAME FILTER).

DESCEND-RELATION is either :DIRECT, or :DEEP.

NAME is a string or :ANY.

FILER is NIL, if no filtering expression was specified for this node. Otherwise, it is a
list. The CAR of this list defines filter type. It could be :RANGE, :PREDICATE,
or :RELATION.

:PREDICATE and :RELATION filters contains one or two parsed JSONPath
expressions. Semantic of JSONPath predicate filter requires existance of specified
path. Relartions compare two expressions, left and right, using a relatioal operator
OP. Right-hand side of a ralation might be a constant, not an JSONPath expression. The
parsed form of relation filter is

  (:RELATION OP-STRING LEFT-PATH-PARSED RIGHT-HAND-PATH-OR-CONSTANT)

Filter path expressions are allowed to use filters as well.

Example: parsing of
  \"$..a[*].b[?(@.x>1)].c[1]\"

yields

    (:ROOT (:DEEP \"a\" (:RANGE :ANY))
           (:DIRECT \"b\" (:RELATION \">\"
                                     (:CURRENT (:DIRECT \"x\" NIL))
                                     1))
           (:DIRECT \"c\" (:RANGE 1 1 1)))
"
  (esrap:parse 'jsonpath jsonpath-expression))
