;;;;
;;;; https://conferences.ieee.org/conferences_events/conferences/search?q=*
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.ieee
  (:use :cl :saexplorer.cfp))

(in-package :saexplorer.cfp.spider.ieee)

(defclass <ieee> (saexplorer.cfp::<cfp-spider>)
  ()
  (:default-initargs :name "ieee"))


(alexandria:define-constant +base-url+ "https://conference-api.ieee.org" :test #'string=)
(alexandria:define-constant +post-url+ "https://conference-api.ieee.org/conf/searchfacet" :test #'string=)
(alexandria:define-constant +main-bundle-js-url+ "https://conferences.ieee.org/conferences_events/main.bundle.js" :test #'string=)

(defun get-api-key (main-bundle-js)
  (aref (nth-value 1 (ppcre:scan-to-strings "confAPIKey:\"(\\w+)\"" main-bundle-js)) 0))

(defun process-json-response (json-response)
  (loop for item in (rest (assoc :results json-response))
    for location = (rest (assoc :location item))
    collect (saexplorer.cfp::make-cfp-reference-info
      :name (rest (assoc :event-title item))
      :location (format nil "~A, ~@[~A, ~]~A"
        (rest (assoc :city location))
        (let ((region (rest (assoc :region location)))) (when (string/= region "") region))
        (rest (assoc :country location)))
      :dates (format nil "~A - ~A"
        (rest (assoc :start-date item))
        (rest (assoc :end-date item)))
      :source-url (format nil "~A~A" +base-url+ (rest (assoc :url item))))))


(defmethod saexplorer.cfp::cfp-collect ((spider <ieee>))
  (loop
    with main-bundle-js = (sa-utils:fetch-url +main-bundle-js-url+)
    with conf-api-key = (get-api-key main-bundle-js)
    for page-number from 0
    for json-response = (cl-json:decode-json-from-string
      (sa-utils:fetch-url +post-url+
        :method :post
        :parameters `(("q" . "*")
                      ("pos" . ,(write-to-string page-number)))
        :headers `(("x-api-key" . ,conf-api-key))))
    for total-results = (rest (assoc :total-results json-response))
    for page-size = (rest (assoc :page-size json-response))
    append (process-json-response json-response)
    while (<= (1+ page-number) (ceiling (/ total-results page-size)))))


(eval-when (:load-toplevel)
  (saexplorer.cfp::register-spider (make-instance '<ieee>)))
