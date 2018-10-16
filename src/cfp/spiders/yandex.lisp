;;;;
;;;; Search for call for papers in Yandex
;;;;

(in-package :cl-user)

(defpackage :saexplorer.cfp.spider.yandex
  (:use :cl)
  (:import-from :saexplorer.cfp
                #:<cfp-spider> #:cfp-collect #:register-spider))

(in-package :saexplorer.cfp.spider.yandex)


(defun make-cfp-query (topic-terms)
  (let ((cfp-terms '("call for papers" "CFP"))
        (conference '("international conference" "workshop" "symposium"))
        ;; format directive that converts a list of strings to a
        ;; string of OR-ed quoted terms, e.g. "ab c" | "e"
        (terms-or-format "~{\"~A\"~^ | ~}")
        (terms-space-format "~{~A~^ ~}"))
    (format nil "+(~@?) && +(~@?) && +(2018 | 2019) && ~@?"
            terms-or-format cfp-terms
            terms-or-format conference
            terms-space-format topic-terms)))

(defun get-document-by-url (url)
  (let* ((content
          (drakma:http-request url :external-format-in :utf-8))
         (doc (plump:parse content)))
    ;; drop all scripts, styles and comments from the document
    (let (comments)
      (plump:traverse doc (lambda (n) (push n comments))
                      :test (lambda (n)
                              (or (plump:comment-p n)
                                  (and (plump:element-p n)
                                       (member (plump:tag-name n) '("script" "style")
                                               :test #'equal)))))
      (mapc #'plump:remove-child comments))

    (dolist (anchor (plump:get-elements-by-tag-name doc "a"))
      (when (plump:has-attribute anchor "href")
        (print (plump:text anchor))
        (print (plump:attribute anchor "href"))))
    (plump:text doc)
    ))



(defun find-cfps (topic-terms)
  "Find Call for Papers on the Internet"
  (let* ((cfp-query (make-cfp-query topic-terms))
         (docs (saexplorer::execute-yandex-query cfp-query)))
    (dolist (doc docs)
      (format t "~A ==> ~A, ~A~%"
              (cdr (assoc :id doc))
              (cdr (assoc :domain doc))
              (cdr (assoc :url doc))))
    #+(or)(get-document-by-url (cdr (assoc :url (nth 19 docs))))))
