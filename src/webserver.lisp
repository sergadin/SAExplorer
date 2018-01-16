;;;

(defpackage :saexplorer.webserver
  (:nicknames :server)
  (:use :cl :websocket-driver)
  (:import-from :cl-log
                #:log-message)
  (:import-from :cl-json
                #:decode-json-from-string)
  (:export #:start-server
           #:add-handler))


(in-package :saexplorer.webserver)

(defvar *clack-handle* nil "Instance of the running clack Web-server.")

(defvar *registered-handlers*
  (cl-containers:make-container 'cl-containers:simple-associative-container :test #'equal)
  "A mapping from URIs to handling applications.")

(defvar *ws-server* nil "Handler of a web sockets server.")


(defun stop-clack ()
  (prog1
      (when *clack-handle*
        (clack:stop *clack-handle*))
    (setf *clack-handle* nil)))

(defun add-handler (path handler)
  (setf (cl-containers:item-at *registered-handlers* path)
        handler))

(defun find-handler (path)
  "Search global registry *REGISTERED-HANDLERS* for a suitable handler
for the PATH specified."
  (flet ((all-positions (char string)
           (loop :for k :from 0 :and item :across string
              :when (char= item char) :collect k)))
    (loop :for k :in (cons nil (reverse (all-positions #\/ path)))
       :for app = (cl-containers:item-at *registered-handlers*
                                         (subseq path 0 (when k (1+ k))))
       :when app
       :do (return app))))


(defun load-static-file (env)
  (let ((path (concatenate 'string
                           (directory-namestring (saexplorer::get-home-directory))
                           "html"
                           (getf env :path-info))))
    (when (open path :direction :probe)
      (list hunchentoot:+http-ok+
            `(:content-type ,(or (hunchentoot:mime-type path) "text/plain"))
            (pathname path)))))


(defun start-server (&key (port 8135))
  (log-message :info "Starting HTTP server on port ~D." port)
  (stop-clack)
  (add-handler "/discover/" #'show-concept-info)
  (setf *clack-handle*
        (clack:clackup
         #'(lambda (env)
             (format t "Requested path is ~A~%" (getf env :path-info))
             (let ((app (find-handler (getf env :path-info)))
                   (static-file (load-static-file env)))
               (handler-case
                   (cond
                     (app (funcall app env))
                     (static-file static-file)
                     (t (list hunchentoot:+http-not-found+
                              '(:content-type "text/html")
                              (list (format nil "No handler for ~A was found.<br/>~%~
                                           Choices are:~%<ul>~{<li><a href=\"~A\">~:*~A</a></li>~%~}</ul>~%"
                                            (getf env :path-info)
                                            (cl-containers:collect-keys *registered-handlers*))
                                    ""))))
               (error (condition)
                 (list hunchentoot:+http-internal-server-error+
                       '(:content-type "text/plain")
                       (list (format nil "~A" condition)
                             ""))))))
         :port port
         :server :hunchentoot))
  ;; Start web sockets server
  (progn
    (setf *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
    (hunchentoot:start *ws-server*)))


(defvar *saexplorer-ws-server*
  (lambda (env)
    (let ((ws (make-server env)))
      (on :message ws
          (lambda (message)
            (send ws message)))
      (lambda (responder)
        (declare (ignore responder))
        (start-connection ws)))))

;;(clack:clackup *saexplorer-ws-server* :server :hunchentoot :port 12345)




(defun show-concept-info (env)
  (destructuring-bind (uri-prefix concept-name . tail)
      (cdr (cl-utilities:split-sequence #\/ (getf env :path-info)))
    (declare (ignore uri-prefix tail))
    `(,hunchentoot:+http-ok+
      (:content-type "text/html")
      (,(format nil "<h3>Explore area: <u>~A</u></h3>" concept-name)
        "Form is here"))))


;;; Web-sockets


(defclass explorer (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this resource!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))
   (state :initform nil :accessor user-state)))

(defgeneric handle-request (resource ws-request user)
  (:documentation "Process USER's request WS-REQUEST to the RESOURCE."))



(defvar *ws-resources* (list (make-instance 'explorer :name "/")))

(defun find-ws-resource (request)
  (log-message :info "Connection to WS resource ~A" (hunchentoot:script-name request))
  (find (hunchentoot:script-name request) *ws-resources* :test #'string= :key #'name))

(defun add-ws-resource (resource)
  (push resource *ws-resources*))

(pushnew 'find-ws-resource hunchensocket:*websocket-dispatch-table*)

(defun broadcast (process message &rest args)
  (loop for peer in (hunchensocket:clients process)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((tp explorer) user)
  (setf (user-state user) :initial)
  #+(or)(broadcast tp "~a has joined ~a" (name user) (name tp)))

(defmethod hunchensocket:client-disconnected ((tp explorer) user)
  (broadcast tp "~a has left ~a" (name user) (name tp)))

(defun sample-progress-bar (client)
  (do ((p 0 (incf p (random 0.1d0))))
      ((> p 1.0d0))
    (log-message :trace "Sending text message: ~A" p)
    (hunchensocket:send-text-message client (json:encode-json-to-string `((:progress . ,p))))
    (sleep (random 1.0d0))))


(defun send-error-message (user message)
  (hunchensocket:send-text-message
   user
   (json:encode-json-alist-to-string
    `((:operation . "Unknown")
      (:category . "error")
      (:message . ,message)))))

(defmethod handle-request :before ((resource explorer) ws-request user)
    (log-message :trace "handle-request called for ~A" (name resource)))

(defmethod handle-request :after ((resource explorer) ws-request user)
    (log-message :trace "Finish processing request for ~A" (name resource)))


(defmethod hunchensocket:text-message-received ((resource explorer) user message-text)
  "Process WebSocket request to a SAExplorer endpoint.

This function validate correctnes of the request, call processing
function and send the result back to the USER.
"
  (log-message :trace "Message recieved: '~A'." message-text)
  (handler-case
      (let ((ws-request (decode-json-from-string message-text)))
        ;;--- TODO: Validate request
        ;; Sending result
        (let ((response (handle-request resource ws-request user)))
          (hunchensocket:send-text-message
           user
           (json:with-explicit-encoder (json:encode-json-to-string response)))
          (log-message :info "Result sent: ~A." (json:encode-json-alist-to-string response))))
    (json:json-syntax-error ()
      (log-message :error "Syntax error.")
      (send-error-message user "Syntax error in the request"))
    (t (e)
      ;;--- FIXME: Catch some errors, not all
      (log-message :error "Unable to process request: ~A" e)
      (send-error-message user "Unable to process the request: internal error.")))
  (log-message :info "Request completed."))
