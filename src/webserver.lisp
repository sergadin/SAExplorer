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
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

(defvar *exploreres* (list (make-instance 'explorer :name "/")))

(defun find-explorer (request)
  (log-message :info "Connection to WS resource ~A" (hunchentoot:script-name request))
  (find (hunchentoot:script-name request) *exploreres* :test #'string= :key #'name))

(pushnew 'find-explorer hunchensocket:*websocket-dispatch-table*)

(defun broadcast (process message &rest args)
  (loop for peer in (hunchensocket:clients process)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((tp explorer) user)
  #+(or)(broadcast tp "~a has joined ~a" (name user) (name tp)))

(defmethod hunchensocket:client-disconnected ((tp explorer) user)
  (broadcast tp "~a has left ~a" (name user) (name tp)))

(defmethod hunchensocket:text-message-received ((tp explorer) user message-text)
  (log-message :info "Message recieved: '~A'." message-text)
  (let* ((message (decode-json-from-string message-text))
         (keywords (cdr (assoc "keywords" message :test #'string-equal))))
    (log-message :info "~A" message)
    (log-message :info "~A" keywords)
    (explorer::find-conferneces (list keywords)))
  (broadcast tp "~a says ~a" (name user) message-text))
