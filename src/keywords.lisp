;;;;
;;;; Various functions to deal with keywords and reltions between them.
;;;;

(defpackage :saexplorer.keywords
  (:nicknames :keywords :kw)
  (:use :cl)
  (:import-from :cl-log
                #:log-message)
  (:import-from :saexplorer
                #:split-keywords)
  (:export #:related))


(in-package :saexplorer.keywords)

(defstruct keyword-description
  (keyword "" :type string)
  (related-keywords nil)
  (subjects nil)
  (distribution-by-years nil))


(defun merge-keyword-descriptions (d1 d2)
  (flet ((merge-lists (x y &key (test #'string=))
           (loop :for (key . nil) :in (union x y :test test :key #'car)
              :collect (cons key
                             (or (cdr (find key x :test test :key #'car))
                                 (cdr (find key y :test test :key #'car))
                                 (error "Empty facet value found while joining keyword descriptions."))))))
    (make-keyword-description
     :keyword (keyword-description-keyword d1)
     :related-keywords (merge-lists (keyword-description-related-keywords d1) (keyword-description-related-keywords d2))
     :subjects (merge-lists (keyword-description-subjects d1) (keyword-description-subjects d2))
     :distribution-by-years (keyword-description-distribution-by-years d1))))

(defun describe-keyword (keyword &key aliases subject (non-recursive t))
  "Execute query to Springer and reformat output as a `keyword-description' structure."
  (let* ((springer-result-json (query-springer (make-query-for-keyword keyword :aliases aliases :subject subject)))
         (direct-description (make-keyword-description
                              :keyword keyword
                              :related-keywords (loop :for (kw . w) :in (extract-facet-values springer-result-json "keyword")
                                                   :collect (cons (normalize-keyword kw :safe-mode t) w))
                              :subjects (extract-facet-values springer-result-json "subject")
                              :distribution-by-years (extract-facet-values springer-result-json "year"))))
    (if (or subject non-recursive)
        direct-description
        ;; run subqueries for all related subjects and merge the results
        (reduce #'merge-keyword-descriptions
                (mapcar (lambda (subject)
                          (describe-keyword keyword :aliases aliases :subject subject))
                        (mapcar #'car (extract-facet-values springer-result-json "subject")))
                :initial-value direct-description))))

(defun related (keywords)
  (describe-keyword (first keyword)))
