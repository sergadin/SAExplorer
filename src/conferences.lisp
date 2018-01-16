;;;;
;;;; Functions related to conferences, e.g. impact
;;;;

(defpackage :saexplorer.conferences
  (:nicknames :confs :conferences)
  (:use :cl)
  (:import-from :cl-log
                #:log-message)
  (:import-from :saexplorer
                #:split-keywords #:gather #:sparse-matchings)
  (:import-from :saexplorer.bibsystem
                #:query #:find-system)
  (:import-from :saexplorer.scopus
                #:proximity)
  (:export #:impact
           #:find-relevant
           #:similar))

(in-package :saexplorer.conferences)

(defclass <conference> ()
  ((abbrev :type string :accessor abbrev)
   (name :type string :accessor name)
   (qualified-name :type string :accessor qualified-name)))



(defun articles-per-year (conference-name)
  (let ((result (query (find-system "Scopus")
                       (format nil "CONFNAME(~A)" (proximity conference-name))
                       :format :json
                       :max-results 20)))
    (format t "~{~A~^~%~}~%" (bibsys:facet-items (bibsys:get-facet result "pubyear")))))

(defun top-authors (conference-name &optional years)
  "Return most frequent authors attending the conference CONFERENCE-NAME in specified YEARS.

The result is formatted as a facet: list of plists with
keys :name :value :hit-count, where name is an author name, value is
an ID, and hit-count is the numeber of papers published by tat author.
"
  (let ((result (query (find-system "Scopus")
                       (format nil "CONFNAME(~A) ~@[AND PUBYEAR(~A)~]"
                               (proximity conference-name)
                               years)
                       :format :json
                       :max-results 20)))
    (let ((top-authors (remove-if #'null
                                  (bibsys:facet-items (bibsys:get-facet result "authname"))
                                  :key #'(lambda (facet-item) (getf facet-item :value)))))
      (format t "~{~A~^~%~}~%" top-authors)
      top-authors)))

(defun author-conferences (author-name &key author-id)
  (log-message :info "Searching conferences for `~A' (au-id: ~A)" author-name author-id)
  (let* ((search-field (if author-id "AU-ID" "AUTHOR-NAME"))
         (search-term (or author-id author-name))
         (result (query (find-system "Scopus")
                        (format nil "~A(~A) AND SRCTYPE(p) AND (PUBYEAR AFT 2000)"
                                search-field search-term)
                        :format :json
                        :max-results 80)))
    (when result
      (let ((conf-names (mapcar #'(lambda (x) (getf x :name))
                                (bibsys:facet-items (bibsys:get-facet result "exactsrctitle")))))
        ;;(format t "~{~A~^~%~}~%" conf-names)
        conf-names))))

(defun extend-abbreviation (proceedings-title-words matching-info)
  "Given a MATCHING-INFO, a plist with
keys :ABBREV, :FIRST, :LAST, :MATCHING, and a list of words if the
proceedings title extends :MATCHING items if possible.

Example:
Itat 2005 Workshop On Theory And Practice Of Information Technologies Applications And Theory Proceedings
"
  matching-info)


(defun reasonable-matching-p (abbrev-info title-words)
  "Verify that the abbreviation described by ABBREV-INFO is linguistically reasonable for TITLE-WORDS."
  (not
   (or (< (length (getf abbrev-info :abbrev))
         3)
      (member (getf abbrev-info :abbrev)
              '("for the")
              :test #'string-equal)
      (member (elt title-words (getf abbrev-info :first))
              '("for" "the" "on" "of" "in")
              :test #'string-equal)
      (member (elt title-words (getf abbrev-info :last))
              '("for" "the" "on" "of" "in")
              :test #'string-equal)
      (< (/ (length (getf abbrev-info :abbrev))
            (getf abbrev-info :length))
         0.7))))


(defun guess-abbreviation (proceedings-title)
  "Guess conference abbreviation and name (two values) from the PROCEEDINGS-TITLE, a string.

Returns two values, abbreviation and its expansion. Example:

'3rd International Conference On Digital Information Management Icdim 2008'
 ==> 'Icdim' 'International Conference On Digital Information Management'
"
  (let* ((title-words (split-sequence:split-sequence #\Space proceedings-title))
         (abbrev-candidates
          ;; List of the form ((:abbrev "ICDS" :length 6 :matching (2 4 6 7) ...) ...)
          (loop with by-first-letter = #'(lambda (x y) (string-equal x y :end1 1 :end2 1))
             for abbrev in title-words
             for abbrev-index from 0
             for pattern = (loop for ch across (string-trim "() " abbrev) collect (format nil "~A" ch))
             for matchings = (mapcar #'(lambda (mat)
                                         (list :abbrev abbrev
                                               :first (first mat)
                                               :last (car (last mat))
                                               :length (1+ (- (car (last mat)) (first mat)))
                                               :matching mat))
                                     (remove-if ; abbrev can not appear in the title!
                                      #'(lambda (mat) (member abbrev-index mat))
                                      (sparse-matchings title-words pattern :test by-first-letter)))
             nconcing matchings))
         (good-candidates
          ;; Exclude all 'short', 'gappy', or stop-word-based abbreviations
          (delete-if-not #'(lambda (m) (reasonable-matching-p m title-words)) abbrev-candidates))
         (best-abbrev-info
          ;; Select the longest match
          (first (sort good-candidates
                       #'<
                       :key #'(lambda (mat) (getf mat :length))))))
    (when best-abbrev-info
      (values (getf best-abbrev-info :abbrev)
              (format nil "~{~A~^ ~}" (subseq title-words
                                              (getf best-abbrev-info :first)
                                              (1+ (getf best-abbrev-info :last))))))))




(defun generalize-name (proceedings-title &key other-titles)
  "Extract generic conference name and abbreviation from its proceedings title.

For example, given a title
'Descriptional Complexity Of Formal Systems 10th International Workshop Dcfs 2008'
the result could be
'Descriptional Complexity Of Formal Systems' and 'Dcfs'.
"
  ;; proceedings-title)
  (declare (ignore other-titles))
  (multiple-value-bind (abbrev full-name)
      (guess-abbreviation proceedings-title)
    (declare (ignore abbrev))
    (or full-name proceedings-title)))


;;
;; number of papers, publication size,
;; authors Hirsh, topic consistency, citations, articles in journals
;;

(defun impact (conference-name)
  (let ((result (query (find-system "Scopus")
                        (format nil "REFSRCTITLE(~A) AND DOCTYPE(ar) AND SRCTYPE(j)"
                                (proximity conference-name))
                        :format :json
                        :max-results 80)))
    (dolist (entry (bibsys:entries result))
      (format t "~A, ~A~%"
              ;;(scopus::get-json-item entry '(:dc\:identifier))
              (scopus::get-json-item entry '(:prism\:issn))
              (scopus::get-json-item entry '(:prism\:publication-name))))
    (loop :for facet :in (bibsys:facets result)
       :do
       (format t "~A~%" (bibsys::name facet))
       (when (string-equal (bibsys::name facet) "exactsrctitle")
         (format t "~A~%" (bibsys:facet-items facet))))
    (articles-per-year conference-name)
    (top-authors conference-name)
    t
    ))


(defun find-relevant (keywords)
  "Return a list of conferences related to specified KEYWORDS, a list of strings."
  (let ((result (query (find-system "Scopus")
                        (format nil "~{KEY(~A)~^ AND ~} AND SRCTYPE(p)" keywords)
                        :format :json
                        :max-results 80)))
    (loop :for facet :in (bibsys:facets result)
       :when (string-equal (bibsys::name facet) "exactsrctitle")
       :nconcing
       (mapcar #'(lambda (x) (getf x :name)) (bibsys:facet-items facet)))))


(defun similar (conference-name)
  "Return list of conferences that are similar to the given one."
  (let ((confs-of-authors
         (loop :for facet-item :in (top-authors conference-name)
            :for author-name = (getf facet-item :name)
            :and author-id = (getf facet-item :value)
            :nconcing (mapcar #'generalize-name
                              (author-conferences author-name :author-id author-id)))))
    (sort
     (mapcar #'(lambda (cluster) (cons (first cluster) (length cluster)))
             (gather confs-of-authors :test #'string-equal))
     #'>
     :key #'cdr)))
