;;;; 'Database' structure for Microsoft academig graph


(require 'lparallel)
(require 'bt-semaphore)

;; (setf lparallel:*kernel* (lparallel:make-kernel 4))

(in-package :saexplorer.tools)


(defvar *organizations-db* nil)
(defvar *authors-db* nil)
(defvar *venue-db* nil)


(defstruct (db-item (:constructor make-db-item (key attributes id)))
  key attributes (count 1) id)

(defclass <names-database> ()
  ((items :accessor names-database-items :initarg :items)
   (counter :accessor names-database-counter :initform 1)
   (semaphore :accessor names-database-semaphore :initarg :semaphore))
  (:default-initargs
   :items (make-hash-table :test 'equal)
    :semaphore (bt-semaphore:make-semaphore :count 1)))

(defgeneric add-name (names-database name-string))
(defgeneric normalize-name (names-database name-string))

(defmethod normalize-name (database name)
  (declare (ignore database))
  (values name nil))

(defmethod add-name :around
    ((names-database <names-database>)
     name-string)
  (declare (ignore name-string))
  (with-accessors ((semaphore names-database-semaphore)
                   (counter names-database-counter))
      names-database
    (bt-semaphore:wait-on-semaphore semaphore)
    (unwind-protect
         (call-next-method)
      (bt-semaphore:signal-semaphore semaphore))))

(defmethod add-name ((names-database <names-database>)
                     name)
  (multiple-value-bind (key attributes) (normalize-name names-database name)
    (let ((item (gethash key (names-database-items names-database) nil)))
      (if item
          (progn
            (incf (db-item-count item))
            item)
          (progn
            (incf (names-database-counter names-database))
            (setf (gethash key (names-database-items names-database))
                  (make-db-item key attributes (names-database-counter names-database))))))))

(defun get-id-for-name (name category)
  (let* ((database (ecase category
                     (:venue *venue-db*)
                     (:organization *organizations-db*)
                     (:author *authors-db*))))
    (db-item-id (add-name database name))))


;; organizations database

(defclass <organizations-database> (<names-database>)
  ())

;; authors databse

(defclass <authors-database> (<names-database>)
  ())

(defclass <venue-database> (<names-database>)
  ())
