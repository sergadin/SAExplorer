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
         (format t "~A~%" (bibsys::items facet))))
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
