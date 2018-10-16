;;;;
;;;; Extraction of dates
;;;;


(in-package :saexplorer.extract)

(defparameter *date-formats*
  '((:month-day-yyyy . "August, 8th 2018")
    (:any . "Any supported format")))

(defparameter *months*
  '("January" "February" "March" "April" "May" "June" "Jule" "August" "September" "October" "November" "December")
  "Full names of months, in English")

(defparameter *days*
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))


(defun get-format-part (date-format-string part)
  (ecase part
    (:month "month")
    (:day "dd")
    (:year "yyyy")))

(defun make-month-regex (date-format-string)
  (alexandria:switch ((get-format-part date-format-string :month) :test #'string=)
    ("month" (format nil "~{~A~^|~}" *months*))
    ("mon" (format nil "~{~A~^|~}" (mapcar (lambda (m) (string-upcase (subseq m 0 3))) *months*)))))

(defun make-day-regex (date-format-string)
  (alexandria:switch ((get-format-part date-format-string :year) :test #'string=)
    ("day" "[0-3][0-9](st|nd|rd|th)")
    ("dd" "[0-3][0-9]")))

(defun make-year-regex (date-format-string)
  (alexandria:switch ((get-format-part date-format-string :year) :test #'string=)
    ("yyyy" "[0-9]{4}")))


(defmacro make-matcher (name regex &body body)
  `(let ((scanner (ppcre:create-scanner ,regex)))
     (lambda (string)
       (cl-ppcre:do-matches
           (begin end scanner string nil :start start)
         (print (subseq string begin end))))))

(defparameter *month-matchers*
  (list
   (make-matcher month "(?i:Jan(uary)?)" 1)
   (make-matcher month "(?i:Feb(ruary)?)" 2)
   (make-matcher month "(?i:Mar(ch)?)" 3)
   (make-matcher month "(?i:Apr(il)?)" 4)
   (make-matcher month "(?i:May)" 5)
   (make-matcher month "(?i:Jun(e)?)" 6)
   (make-matcher month "(?i:Jul(y)?)" 7)
   (make-matcher month "(?i:Aug(ust)?)" 8)
   (make-matcher month "(?i:Sep(tember)?)" 9)
   (make-matcher month "(?i:Oct(ober)?)" 10)
   (make-matcher month "(?i:Nov(ember)?)" 11)
   (make-matcher month "(?i:Dec(ember)?)" 12)))

(defparameter *date-regexes*
  '(:month-day-yyyy "(?i:January|February|March|April|May|June|July|August|September|October|November|December?), [0-9]{1,2}(st|nd|rd|th) [0-9]{4}"))


(defun dates (string &key (start 0) (format :any))
  "Locate first date description in STRING starting from START."
  (cl-ppcre:do-matches
      (begin end (getf *date-regexes* :month-day-yyyy) string nil :start start)
    (print (subseq string begin end)))
  nil)


(defun all-dates (text &key (start 0) (format :any))
  "Find all dates in a given string TEXT. If FORMAT equals :ANY, which is the
default, then all possible dates are extracted."
  (dates text :start start :format format))
