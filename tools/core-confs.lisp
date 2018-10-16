;;;;
;;;; Load CORE
;;;; http://portal.core.edu.au/conf-ranks/?search=&by=all&source=all&sort=atitle&page=34
;;;;
;;;; Main function is `load-core-conferences'.
;;;;

(in-package :saexplorer.tools)

;;;
;;; Sequence No, Title, Acronym, Source, Class, HasData, PrimaryFoR, No of Comments, Average rating
(defun load-core-conferences (csv-filename)
  (cl-csv:do-csv (row csv-filename)
    (print row)))


(defun load-anzsrc-for (rdf-json-filename)
  (cl-json:decode-json-from-source #P"c:/Users/serg/VirtualBox_Shared/Projects/SAExplorer/local/data/anzsrc-for_2008.json")
  t)
