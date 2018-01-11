(in-package :saexplorer)


(defun split-keywords (str &key delimiter)
  "Guesses a delimiter and splits keywords given by STR string."
  ;; Not so smart...
  (mapcar #'(lambda (kw)
              (string-trim #(#\Space) kw))
          (split-sequence:split-sequence (or delimiter #\|) str)))

#|
  (loop :for index = 0 :then space-at
     :for space-at = (position #\Space str :start (+ 1 index))
     :while space-at
     :collect space-at)
|#


(defun normalize-keyword (keyword &key safe-mode)
  "Remove double spaces and other deficiencies in KEYWORD string."
  (let ((rules (list (cons "[\-]" " ")))   ; remove dashes
        (safe-rules (list (cons "[\\n|\\r\\t]+" " ") ; new lines and tabs as spaces
                          (cons " [ ]+" " ") ; remove long spaces
                          (cons "^[ ]+" "") ; remove leading spaces
                          (cons "[ ]+$" "")))) ; remove trailing spaces
    (reduce #'(lambda (s rule)
                (cl-ppcre:regex-replace-all (car rule) s (cdr rule)))
            (if safe-mode
                safe-rules
                (append rules safe-rules))
            :initial-value (funcall (if safe-mode #'identity #'string-upcase) keyword))))
