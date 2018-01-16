;;;
;;; Processing WebSockets requests to conference resource
;;;

(in-package :saexplorer.wsresources)

(defclass <conference-resource> (saexplorer.webserver::explorer)
  ()
  (:default-initargs :name "/conference/"))


;; Register the resource
(add-ws-resource (make-instance '<conference-resource>))

;;;
;;; process-... functions are just interfaces between underlying
;;; classes and the format used in web sockets part. Each function
;;; evaluates procedure and converts its result into an expression
;;; suitable for JSON-encoding into 'data' field of the websocket
;;; response message, using `json:with-explicit-encoder' macro.
;;;

(defun process-find (ws-request user)
  "Process find request."
  (declare (ignore user))
  (let ((keywords (cdr (assoc "keywords" ws-request :test #'string-equal))))
    `(:plist :entries (:list ,@(mapcar #'(lambda (title) `(:plist :title ,title))
                                       (confs:find-relevant keywords))))))

(defun process-similar (ws-request user)
  (declare (ignore user))
  (let* ((conference-name (cdr (assoc "confname" message :test #'string-equal)))
         (response (confs:similar conference-name)))
    `(:alist (:similar-conferences . ,response))))

(defun process-impact (ws-request user)
  (declare (ignore user))
  (let* ((conference-name (cdr (assoc "confname" message :test #'string-equal)))
         (response (confs:impact conference-name)))
    `(:alist (:conference-impact . ,response))))

;;;
;;; Requests dispatcher. Determines and calls correct process-
;;; function.
;;;

(defmethod handle-request ((resource <conference-resource>) ws-request user)
  (list
   :plist
   :result
   (alexandria:switch ((cdr (assoc "operation" ws-request :test #'string-equal))
                       :test #'string-equal)
     ("find" (process-find ws-request user))
     ("similar" (process-similar ws-request user))
     ("impact" (process-impact ws-request user))
     ("describe" '((:error-message . "Not implemented"))))))
