(in-package :saexplorer.bibsystem)

;;;
;;; Domain-specific documents
;;;

(defclass <publication-document> (<document>)
  ()
  (:documentation "System-independent representation of a publication.")
  (:default-initargs
      :valid-properties
      `((:title . "Title of the publication")
        (:abstract . "Abstract")
        (:authors . "Authors")
        (:keywords . "Authors leywords")
        (:lang . "Language of the publication")
        (:start-page . "First page")
        (:end-page . "Last page")
        (:year . "Publication year")
        (:venue-name . "Source title, e.g. journal title, or conference name")
        (:doi . "doi")
        (:fulltext-url . "Full text url"))))

(defclass <author-document> (<document>)
  ()
  (:documentation "System-independent representation of an author."))
