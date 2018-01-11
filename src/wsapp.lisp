;;;
;;; Websocket application framework on top of websocket-driver.
;;;
;;; Define `websocket-resource` and default clack application that
;;; forwards requests to relevant resources.
;;;

(defpackage :saexplorer.wsapp
  (:nicknames :wsapp)
  (:use :cl :websocket-driver)
  (:import-from :cl-log
                #:log-message)
  (:export #:start-server
           #:add-handler))

(in-package :saexplorer.wsapp)

(defclass websocket-resource ()
  ()
  (:documentation "A server may provide number of resources. Each
  resuurse is associated with some URL. Resource instance serves all
  requests on the server for that particular url."))


(defgeneric resource-client-connected (resource client)
  (:documentation "Called when a client finishes connecting to a
WebSockets resource, and data can be sent to the client."))

(defgeneric resource-client-disconnected (resource client)
  (:documentation "Called when a client disconnected from a WebSockets resource."))

(defgeneric resource-received-text (resource client message)
  (:documentation "Called when a client sent a text message to a WebSockets resource."))

(defgeneric resource-received-binary (resource client message)
(:documentation "Called when a client sent a binary message to a WebSockets resource."))


(defclass websocket-client ()
  ())


(defun make-clack-app ()
  (lambda (env)
    (let ((ws (websocket-driver:make-server env)))
      (websocket-driver:on :message ws
          (lambda (message)
            (send ws message)))
      (lambda (env)
        (let ((path-info (getf env :path-info)))
          (declare (ignore path-info))
          (websocket-driver:start-connection ws))))))
