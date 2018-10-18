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
         :accessor cfp-name)
   (acronym :col-type (or (:varchar 32) :null)
            :initarg :acronym
            :accessor cfp-acronym)
   (year :col-type (or :integer :null)
         :initarg :year
         :accessor cfp-year)
   (dates :col-type (or (:varchar 512) :null)
        :initarg :dates
        :accessor cfp-dates)
   (deadline :col-type (or (:varchar 512) :null)
        :initarg :deadline
        :accessor cfp-deadline)
   (location :col-type (or (:varchar 512) :null)
        :initarg :location
        :accessor cfp-location)
   (conftype :col-type (or <conf-type> :null)
             :initarg :conftype
             :accessor cfp-conftype)
   (url :col-type (:varchar 512)
        :initarg :url
        :accessor cfp-url)
   (source :col-type (:varchar 512)
           :initarg :source
           :accessor cfp-source)
   (source-url :col-type (or (:varchar 512) :null)
               :initarg :source-url
               :accessor cfp-source-url))
  (:metaclass mito:dao-table-class)
  (:table-name "cfp")
  (:unque-keys (name entity))
  (:documentation "Description of Call for Papers page. Includes
conference name, dates, url, and information about reference
location."))
