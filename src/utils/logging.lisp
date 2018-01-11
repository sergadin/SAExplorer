;;; Configure logging facility

(in-package :saexplorer)

(defun setup-logging ()
  (cl-log:defcategory :critical)
  (cl-log:defcategory :error   (or :error :critical))
  (cl-log:defcategory :warning (or :warning :error))
  (cl-log:defcategory :notice  (or :notice :warning))
  (cl-log:defcategory :info    (or :info :notice))
  (cl-log:defcategory :trace   (or :debug :info))
  (cl-log:defcategory :debug   (or :debug :info))

  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'cl-log:formatted-message))

  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :stream *standard-output*
                          ;;:category '(or :info)))
                          :category '(or :debug :info :trace)))


(defmethod cl-log:format-message ((self cl-log:formatted-message))
  (flet ((format-timestamp (timestamp)
           (let ((ut (cl-log:timestamp-universal-time timestamp))
                 (fraction (cl-log:timestamp-fraction timestamp)))
             (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                 (decode-universal-time ut)
               (declare (ignore dow dst-p tz))
               (declare (ignore yr mon day))
               (concatenate 'string
                            ""
                            ;(format nil "~4,'0d-~2,'0d-~2,'0d " yr mon day)
                            (format nil "~2,'0d:~2,'0d:~2,'0d.~6,'0d" hr min sec fraction))))))
  (format nil "~a ~a ~?~&"
          (format-timestamp (cl-log:message-timestamp self))
          (cl-log:message-category self)
          (cl-log:message-description self)
          (cl-log:message-arguments self))))
