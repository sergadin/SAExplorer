;;;;
;;;;  Collecting new URL references to call for papres.
;;;;

(in-package :saexplorer.cfp)

(defvar *cfp-spiders* (make-hash-table :test #'equal) "Regestry of available CFP sources.")

(defclass <cfp-spider> ()
  ((name :type string
         :initarg :name
         :accessor cfp-spider-name
         :initform (error "Must supply cfp-spider name.")
         :documentation "Spiders are distinguished by names, unique short lables of CFP sources.")))


(defun make-cfp-reference-info (&key name acronym year dates deadline location url source source-url)
  (make-instance
   'saexplorer.models:<cfp-page>
   :name name
   :acronym acronym
   :year year
   :dates dates
   :deadline deadline
   :location location
   :url url
   :source source
   :source-url source-url))


(defgeneric cfp-collect (spider)
  (:documentation "Collect presumably new CFP references using SPIDER.

This method should return a list of `saexplorer.models:<cfp-page>'
objects. Such object may be created by `make-cfp-reference-info'
helper function.
"))

(defgeneric cfp-explain (spider cfp)
  (:documentation "Provide more information about specified cfp.")
  (:method ((spider t) cfp)
    (declare (ignore spider))
    cfp))

(defgeneric cfp-rescan (spider)
  (:documntation "Perform complete rescan of the resource.")
  (:method ((spider <cfp-spider>))
    "By default, just call `cfp-collect'."
    (cfp-collect spider)))


(defun register-spider (spider)
  (setf (gethash (cfp-spider-name spider) *cfp-spiders*) spider))

(defun create-cfp (cfp source)
  (setf (cfp-source cfp) source)
  (mito:insert-dao cfp))


(defun new-call-for-papers-p (cfp source)
  "Check that the CFP was not collected from this source."
  (and (cfp-name cfp)
       (null (mito:find-dao '<cfp-page>
                            :name (cfp-name cfp)))))


(defun collect (&optional source-name)
  "Collect new call for papers. If NAME is provided, collect from the specified source only."
  (if source-name
      (alexandria:when-let
          ((spider (gethash source-name *cfp-spiders*))
           (new-cfp-count 1)
           (cfp-count 1))
        (log-message :trace "Collecting CFP from ~A" source-name)
        (dolist (cfp (cfp-collect spider))
          (incf cfp-count)
          (when (new-call-for-papers-p cfp source-name)
            (incf new-cfp-count)
            (create-cfp (cfp-explain spider cfp) source-name)))
        (log-message :info "~D CFP added out of ~D." (1- new-cfp-count) (1- cfp-count)))
      ;; no source name specified, iterate over all registered spiders
      (maphash #'(lambda (name spider)
                   (declare (ignore spider))
                   (collect name))
               *cfp-spiders*)))
