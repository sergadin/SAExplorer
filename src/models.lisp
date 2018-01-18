;;;;
;;;; Models for database tables.
;;;;

(in-package :cl-user)

(defpackage saexplorer.models
  (:nicknames :models)
  (:use :cl)
  (:export #:<bibdb> #:<entity>
           #:<conf-type> #:<conf-info>))


(defclass <bibdb> ()
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


(defclass <entity> ()
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


(defclass <conf-type> ()
  ((code :col-type (:varchar 4)
         :primary-key t
         :initarg :code
         :accessor conf-type-code)
   (name :col-type (:varchar 128)
         :initarg :name
         :accessor conf-type-name))
  (:metaclass mito:dao-table-class)
  (:table-name "conftype")
  (:unique-keys code)
  (:documentation "Conference types."))

(defclass <conf-info> ()
  ((name :col-type (:varchar 512)
         :initarg :name
         :accessor conf-name)
   (acronym :col-type (or (:varchar 32) :null)
            :initarg :acronym
            :accessor conf-acronym)
   (conftype :col-type <conf-type>
             :initarg :conftype
             :accessor conf-conftype)
   (entity :col-type <entity>
           :initarg :entity
           :accessor conf-entity))
  (:metaclass mito:dao-table-class)
  (:table-name "conference")
  (:unque-keys (name entity))
  (:documentation "COnference names and acronyms."))
