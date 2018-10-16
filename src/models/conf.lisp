;;;;
;;;; Tables related to conferences.
;;;;

(in-package :saexplorer.models)

(defclass <conf-type> (<model>)
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

(defclass <conf-info> (<model>)
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
  (:documentation "Conference names and acronyms."))



(defclass <cfp-page> (<model>)
  ((name :col-type (:varchar 512)
         :initarg :name
         :accessor conf-name)
   (acronym :col-type (or (:varchar 32) :null)
            :initarg :acronym
            :accessor conf-acronym)
   (year :col-type (or :integer :null)
         :initarg :year
         :accessor conf-year)
   (conftype :col-type (or <conf-type> :null)
             :initarg :conftype
             :accessor conf-conftype)
   (url :col-type (:varchar 512)
        :initarg :url
        :accessor conf-url)
   (source :col-type (:varchar 512)
           :initarg :url
           :accessor cfp-source)
   (source-url :col-type (:varchar 512)
               :initarg :url
               :accessor cfp-source-url))
  (:metaclass mito:dao-table-class)
  (:table-name "cfp")
  (:unque-keys (name entity))
  (:documentation "Call for Papers."))
