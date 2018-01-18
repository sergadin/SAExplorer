;;;;
;;;; Models for database tables.
;;;;

(in-package :saexplorer.models)

(defclass <model> ()
  ()
  (:documentation "The base class for all models."))

(defclass <bibdb> (<model>)
  ((code :col-type (:varchar 4)
         :initarg :code
         :accessor bibdb-code
         :documentation "Unique codename of the database.")
   (name :col-type (:varchar 128)
         :initarg :name
         :accessor bibdb-name))
  (:metaclass mito:dao-table-class)
  (:table-name "bibdb")
  (:unique-keys code)
  (:documentation "Bibliography database."))


(defclass <entity> (<model>)
  ((category :col-type (:varchar 32)
             :initarg :category
             :accessor entity-category)
   (bibdb :col-type <bibdb>
          :initarg :bibdb
          :accessor entity-bibdb)
   (identifier :col-type (:varchar 128)
               :initarg :identifier
               :accessor entity-identifier))
  (:metaclass mito:dao-table-class)
  (:table-name "entity")
  (:unique-keys (bibdb identifier))
  (:documentation "Information about identifier in a system."))
