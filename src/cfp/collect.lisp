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

(defstruct (cfp-reference-info (:conc-name cfp-))
  "Description of Call for Papers page reference, including conference
name, dates, url and information about reference location."
  name acronym year dates location url source source-url)


(defgeneric cfp-collect (spider)
  (:documentation "Collect CFP references using SPIDER."))

(defgeneric cfp-explain (spider cfp)
  (:documentation "Provide more information about specified cfp.")
  (:method ((spider t) cfp)
    (declare (ignore spider))
    cfp))


(defun register-spider (spider)
  (setf (gethash (cfp-spider-name spider) *cfp-spiders*) spider))


(defun create-cfp (cfp source)
  (mito:create-dao '<cfp-page>
                   :name (cfp-name cfp)
                   ;; :conftype conftype
                   :url (cfp-url cfp)
                   :source source))

(defun new-call-for-papers-p (cfp source)
  "Check that the CFP was not collected from this source."
  (and (cfp-name cfp)
       (null (mito:find-dao '<cfp-page>
                            :name (cfp-name cfp)))))


(defun collect (&optional source-name)
  "Collect new call for papers. If NAME is provided, collect from the specified source only."
  (if source-name
      (let ((spider (gethash source-name *cfp-spiders*)))
        (when spider
          (log-message :trace "Collecting CFP from ~A" source-name)
          (dolist (cfp (cfp-collect spider))
            (if (new-call-for-papers-p cfp source-name)
                (create-cfp (cfp-explain spider cfp) source-name)
                (log-message :info "Old conference: ~A" (cfp-name cfp))))))
      (maphash #'(lambda (name spider)
                   (declare (ignore spider))
                   (collect name))
               *cfp-spiders*)))
