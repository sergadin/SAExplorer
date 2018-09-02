;;;; API for ElasticSearch server


(in-package :saexplorer.tools)


(defparameter *es-host* "localhost")
(defparameter *es-port* "9200")

(defparameter *es-index* "mag")
(defparameter *es-type* "article")

(defun es-connect (host &key (port 9200) index type)
  "'Connect' to ElasticSearch server. Subsequent calls to ES API
functions will use parameters provided to this function."
  (setf *es-host* host)
  (setf *es-port* (string port))
  (setf *es-index index)
  (setf *es-type* type))


(defun es-put (index type pk json-content)
  (drakma:http-request (format nil "http://~A:~A/~A/~A/~A/_create" *es-host* *es-port* index type pk)
                       :method :PUT
                       :content-type "application/json"
                       :content json-content))

(defun es-get (index type id)
  (multiple-value-bind (content status-code server-headers)
      (drakma:http-request (format nil "http://~A:~A/~A/~A/~A/_source" *es-host* *es-port* index type id)
                           :method :GET)
    (flexi-streams:octets-to-string content :external-format :utf-8)))

(defun es-delete (index type query)
  (multiple-value-bind (content status-code server-headers)
      (drakma:http-request (format nil "http://~A:~A/~A/~A/_query" *es-host* *es-port* index type)
                           :method :DELETE
                           :content-type "application/json"
                           :content query)
    (flexi-streams:octets-to-string content :external-format :utf-8)))

(defun es-index-create (index number-of-shards number-of-replicas)
  (let ((config
         (json:with-explicit-encoder
           (json:encode-json-to-string
            `(:plist :settings
                     (:plist :index
                             (:plist :number_of_shards ,number-of-shards
                                     :number_of_replicas ,number-of-replicas)))))))
    (multiple-value-bind (content status-code server-headers reply-uri stream close-stream reason)
        (drakma:http-request (format nil "http://~A:~A/~A" *es-host* *es-port* index)
                             :method :PUT
                             :content-type "application/json"
                             :content config)
      (flexi-streams:octets-to-string content :external-format :utf-8))))

(defun es-index-delete (index)
  (drakma:http-request (format nil "http://~A:~A/~A" *es-host* *es-port* index)
                       :method :DELETE))
