;;;;
;;;; Caching utilities.
;;;;

(in-package :cl-user)

(defpackage saexplorer.cache
  (:nicknames :cache)
  (:use :cl :saexplorer.sys)
  (:export #:with-cached-result))

(in-package :saexplorer.cache)

(defparameter *default-cache-timeout* (* 60 60 24 30) "Default cache timeout, in seconds (one month).")

(defmacro with-cached-result ((content-var cache-key &key timeout) update-body &body body)
  "Execute BODY as an implicit PROGN, bind the rusult as CONTENT-VAR
and put the result into cache under key CACHE-KEY. If CACHE-KEY is
found in the cache, do not evaluate BODY and use previously cached
value.
"
  (let ((exists-p (gensym "EXISTS-P-")))
    `(let* ((,exists-p (red:exists ,cache-key))
            (,content-var (if ,exists-p
                              (red:get ,cache-key)
                              ,update-body)))
       (unless ,exists-p
         (red:setex ,cache-key (or ,timeout ,*default-cache-timeout*) ,content-var))
       ,@body)))



#|
"
(with-cached-result (content \"key\" :timeout 60)
    (drakma:http-request ...)
  (print content)
  (parse content))

"
|#
