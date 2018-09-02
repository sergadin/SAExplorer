;;;; Iterate over all records in all MAG files matching name template.

(in-package :saexplorer.tools)


(defun %process-single-file (filename item-processor &key test (lines-limit 2000) (log-step 100000) output-writer)
  "Call LINE-PRECESSOR for each line of FILENAME.

ITEM-PROCESSOR should accept (json filename position string-length).
"
  (flet ((consume-processed (channel &key timeout)
           "Consume results from the CHANNEL and call OUTPUT-WRITER if needed."
           (loop for output-data = (lparallel:try-receive-result channel :timeout timeout)
              while output-data
              do (when output-writer (funcall output-writer output-data))))
         (caller (data-line filename stream-position)
           "Call ITEM-PROCESSOR when json parsed from DATA-LINE satisfies TEST."
           (let ((json (ignore-errors (json:decode-json-from-string data-line))))
             (when (or (null test) (funcall test json))
               (funcall item-processor json filename stream-position (length data-line)))))
         (submit-task (channel fn data-line filename stream-position)
           (lparallel:submit-task channel fn data-line filename stream-position)))
    (with-open-file (f filename)
      (loop
         with number-of-kernels = 4 and non-parallel-processing = t
         with channels = (loop for k below number-of-kernels collect (lparallel:make-channel))
         for stream-position = (file-position f)
         for data-line = (read-line f nil nil) and line-number from 0
         for channel = (elt channels (mod line-number number-of-kernels))
         while (and data-line (or (null lines-limit) (< line-number lines-limit)))
         when (zerop (mod line-number log-step))
         do (cl-log:log-message :trace "~A records processed so far." line-number)
         do
           (if non-parallel-processing
               (let* ((json (ignore-errors (json:decode-json-from-string data-line)))
                      (output-data (funcall item-processor json filename stream-position (length data-line))))
                 (when (and output-writer output-data)
                   (funcall output-writer output-data)))
               ;; Use lparallel
               (progn
                 ;; consume computed result before submitting new task to the channel
                 (consume-processed channel)
                 (submit-task channel #'caller data-line filename stream-position)))
         finally
           (dolist (channel channels)
             (consume-processed channel :timeout 1.0))))))



(defun process-mag-files (directory-name &key (pattern "mag*.txt") record-filter record-processor output-writer lines-limit)
  "Process all matching files in DIRECTORY-NAME."
  (dolist (path (directory (format nil "~A/**/~A" directory-name pattern)))
    (let ((cutoff (local-time:now)))
      (cl-log:log-message :trace "Loading ~A" (pathname-name path))
      (%process-single-file path record-processor
                            :test record-filter
                            :output-writer output-writer
                            :lines-limit lines-limit)
      (cl-log:log-message :trace "File ~A LOADED in ~A." (pathname-name path)
                          (local-time-duration:human-readable-duration
                           (local-time-duration:timestamp-difference (local-time:now) cutoff))))))
