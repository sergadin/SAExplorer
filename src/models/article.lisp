;;;;
;;;; MITO models related to articles.
;;;;

(in-package :saexplorer.models)

(defclass <article> (<model>)
  ((code :col-type (:varchar 4)
         :primary-key t
         :initarg :code
         :accessor conf-type-code)
   (name :col-type (:varchar 128)
         :initarg :name
         :accessor conf-type-name))
  (:metaclass mito:dao-table-class)
  (:table-name "article")
  (:unique-keys code)
  (:documentation "Article."))
