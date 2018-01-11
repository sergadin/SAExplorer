;;;; Query  builder

(in-package :saexplorer.bibsystem)

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
