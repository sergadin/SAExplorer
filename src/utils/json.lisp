(in-package :saexplorer)

(defun get-json-item (json keys-path)
  "Extract value of an item specified by a sequence of json keys."
  (reduce #'(lambda (sub-json key)
              (cdr (assoc key sub-json)))
          (if (listp keys-path) keys-path (list keys-path))
          :initial-value json))
