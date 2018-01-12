;;;;
;;;; Functions related to conferences, e.g. impact
;;;;

(defpackage :saexplorer.conferences
  (:nicknames :confs :conferences)
  (:use :cl)
  (:import-from :cl-log
                #:log-message)
  (:import-from :saexplorer
                #:split-keywords)
  (:import-from :saexplorer.bibsystem
                #:query #:find-system)
  (:import-from :saexplorer.scopus
                #:proximity)
  (:export #:impact
           #:find-relevant))

(in-package :saexplorer.conferences)


(defun articles-per-year (conference-name)
  (let ((result (query (find-system "Scopus")
                       (format nil "CONFNAME(~A)" (proximity conference-name))
                       :format :json
                       :max-results 20)))
    (format t "~{~A~^~%~}~%" (bibsys:facet-items (bibsys:get-facet result "pubyear")))))

(defun top-authors (conference-name &optional years)
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


(defun generalize-name (proceedings-title)
  "Extract generic conference name and abbreviation from its proceedings title.

For example, given a title
'Descriptional Complexity Of Formal Systems 10th International Workshop Dcfs 2008'
the result could be
'Descriptional Complexity Of Formal Systems' and 'Dcfs'.
"
  proceedings-title)


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
  (let* ((q-str (format nil "~{~A~^ AND ~}"
                        (list (format nil "(AUTHKEY(~{~A~^ OR ~}) OR KEY(~{~A~^ OR ~}))" keywords keywords)
                              "SRCTYPE(p)")))
         (entries (mapcan #'(lambda (chunk-content)
                              (scopus::get-json-item (cl-json:decode-json-from-string chunk-content)
                                                     '(:search-results :entry)))
                         (scopus::query-scopus (print q-str) :max-results 1000))))
    (print (scopus->facets entries))
    (dolist (entry entries)
      (format t "~A~%" (scopus::get-json-item entry '(:prism\:publication-name)))
      ;;(format t "~{~A~^, ~}~%" (split-keywords (scopus::get-json-item entry '(:authkeywords))))
      #+(or)(format t "~A~%" (scopus::get-json-item entry '(:prism\:cover-date))))))

(defun similar (conference-name)
  "Return list of conferences that are similar to the given one."
  (loop :for facet-item :in (top-authors conference-name)
     :for author-name = (getf facet-item :name)
     :and author-id = (getf facet-item :value)
     :nconcing (mapcar #'generalize-name
                       (author-conferences author-name :author-id author-id))))
