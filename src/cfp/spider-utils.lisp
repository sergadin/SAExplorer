;;;; Collection of useful tools for crawling

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider-utils
  (:nicknames :spider-utils)
  (:use :cl)
  (:export #:<depaginator>
           #:safe-first))

(in-package :saexplorer.cfp.spider-utils)

(defvar *cached* (make-hash-table :test #'equal) "Store cached documents while debugging.")

(defclass <depaginator> ()
  ((maximum-number-of-pages
    :type integer
    :initarg :maximum-pages
    :documentation "A constant to avoid infinite loops. Spider will
  stop processing after reaching this limit.")
   (initial-page-number
    :type integer
    :initarg :initial-number
    :documentation "Initial number of page counter.")
   (initial-url
    :type string
    :initarg :initial-url
    :documentation "URL or parameterized URL template for single-list sites. See `initial-pages' generic function."))
  (:default-initargs :maximum-pages 200 :initial-number 1)
  (:documentation ""))

(defgeneric initial-pages (depaginator)
  (:documentation "Returns sequence of objects passed to `page-url', a kind of 'sections'.")
  (:method ((depaginator <depaginator>))
    (when (slot-boundp depaginator 'initial-url)
      (list (slot-value depaginator 'initial-url)))))

(defgeneric page-url (depaginator current-template page-number)
  (:documentation "Generate URL to retrieve content of the page by its PAGE-NUMBER."))

(defgeneric process-single-page (depaginator parsed-page-conent)
  (:documentation "Extract all information to be collected form the specified PARSED-PAGE-CONTENT."))

(defgeneric next-url (depaginator parsed-page-conent)
  (:documentation "Return next URL, or NIL, if the curent page is the last one."))


(defgeneric process (depaginator)
  (:method ((depaginator <depaginator>))
    (loop :for url-template :in (initial-pages depaginator)
       :append
       (loop
          :for page-number :from (slot-value depaginator 'initial-page-number)
          :for url = (page-url depaginator url-template page-number)
          :for doc-root = (plump:parse #+cached-http-requests (cached-fetch-url url)
                                       #-cached-http-requests (sa-utils:fetch-url url))
          :append (process-single-page depaginator doc-root)
          :while (and (< page-number (slot-value depaginator 'maximum-number-of-pages))
                      (next-url depaginator doc-root))))))

;;;
;;; Parsing utils
;;;

(defun safe-first (arr)
  "Extract first element from the array ARR, or return NIL, if ARR is not an array, or empty."
  (when (and (arrayp arr) (= 1 (array-rank arr)) (< 0 (length arr)))
    (aref arr 0)))

(defun cached-fetch-url (url)
  "A replacement of `sa-utils:fetch-url' that allows to cache HTML code in special variable *cached*."
  (let ((html (gethash url *cached*)))
    (or html
        (setf (gethash url *cached*) (sa-utils:fetch-url url)))))
