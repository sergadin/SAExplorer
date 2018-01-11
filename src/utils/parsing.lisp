(in-package :saexplorer)

(defun dehtml-string (string)
  "Replace all percent-encoded and ampersand encoded constants of the form &amp; by corresponding characters."
  (urlencode:urldecode (html-entities:decode-entities string) :lenientp t :queryp t))


(defun url-encode (string external-format)
  "Returns a URL-encoded version of the string STRING using the
external format EXTERNAL-FORMAT."
  (with-output-to-string (out)
    (loop for octet across (drakma::string-to-octets (or string "")
                                             :external-format external-format)
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "$-_.!*'(),<>" :test #'char=))
                     (write-char char out))
                   ((char= char #\Space)
                     (write-char #\+ out))
                   (t (format out "%~2,'0x" (char-code char)))))))

(defun clean-doi (string)
  "Remove http://dx.doi.org/ or doi: prefix form `STRING`, if present."
  (let ((re-site-doi "(?i)^((.*http://dx.doi.org/)|(.*doi:))?(10.*)$"))
    (or
     ;; correct doi string
     (ppcre:register-groups-bind (host scheme prefix doi)
         (re-site-doi string)
       (declare (ignorable prefix host scheme doi))
       doi)
     ;; unrecognized format: leave as is
     string)))
