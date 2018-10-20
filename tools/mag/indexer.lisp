;;;; Process Microsoft academic graph JSON data

(in-package :saexplorer.tools)


;;; Binary encoding

(defconstant +identifier-bit-length+ (* 8 16))


(defun encode-object-identifier (id-string)
  "Convert identifier like 0003b4a4-fc67-489e-8b2a-7495afe99e80 to a bignum."
  (parse-integer (delete #\- id-string) :radix 16))

(defun encode-mag-record (json)
  (let* ((id (cdr (assoc :id json)))
         (authors (cdr (assoc :authors json)))
         (year (or (cdr (assoc :year json)) 1900))
         (references (cdr (assoc :references json)))
         (vec (make-array (+ (/ +identifier-bit-length+ 8) ; id
                             4 ; number of authors
                             (* 4 2 (length authors)) ; authors
                             4 ; number of references
                             (* (/ +identifier-bit-length+ 8) (length references)) ; references
                             1) ; year
                          :adjustable nil
                          :element-type '(unsigned-byte 8)))
         (write-position 0))
    (flet ((store-identifier (identifier &optional (bit-size +identifier-bit-length+))
             (do ((pos 0 (+ 8 pos)))
                 ((>= pos bit-size))
               (setf (aref vec write-position) (ldb (byte 8 pos) identifier))
               (incf write-position))))
      (store-identifier (encode-object-identifier id) +identifier-bit-length+)
      ;; authors: <n-authors> <au-name> <au-organization>
      (store-identifier (length authors) 32)
      (loop for author in authors
         for name = (cdr (assoc :name author))
         and org = (cdr (assoc :org author))
         do
           (store-identifier (get-id-for-name name :author) 32)
           (store-identifier (get-id-for-name org :organization) 32))
      ;; references: <len> <ref-1> ... <ref-n>
      (store-identifier (length references) 32)
      (dolist (ref-id references)
        (store-identifier (encode-object-identifier ref-id) +identifier-bit-length+))
      ;; year, 1900-based
      (setf (aref vec write-position) (- year 1900))
      (incf write-position))
    vec))


(defun put-json-in-binary (data stream)
  (fast-io:with-fast-output (buffer stream)
    (fast-io:fast-write-sequence data buffer)))


;;; main part

(defun reset-mag ()
  (setf *organizations-db* (make-instance '<organizations-database>))
  (setf *venue-db* (make-instance '<venue-database>))
  (setf *authors-db* (make-instance '<authors-database>)))

;; (setf lparallel:*kernel* (lparallel:make-kernel 4))
;; (es-index-create *es-index* 1 0)


(defun process-line (json &key (test #'identity) stream-position)
  (let* ((venue (cdr (assoc :venue json))))
    (when (and venue
               (string= (cdr (assoc :doc--type json)) "Conference"))
      (add-name *venue-db* venue))
    (loop for author in (cdr (assoc :authors json))
       for name = (cdr (assoc :name author))
       and org = (cdr (assoc :org author))
       do (add-name *authors-db* name)
         (add-name *organizations-db* org))
    #+(or)(let ((polished-json (remove-if-not
                                #'(lambda (p)
                                    (member (car p)
                                            '(:doc--type :id
                                              :authors :title :venue :year
                                              :keywords :fos :lang
                                              :references)))
                                json)))
            (encode-mag-record polished-json))
    nil))


(defun save-polished-to-es (json-string &key (test #'identity) stream-position)
  "Save data to ElasticSearch."
  (declare (ignore test stream-position))
  (let* ((json (ignore-errors (json:decode-json-from-string json-string)))
         (polished-json (remove-if-not
                         #'(lambda (p)
                             (member (car p) '(:id :authors :title :venue :fos :references :lang :keywords :year)))
                         json))
         (polished-json-string (json:encode-json-to-string polished-json)))
    (when (and json)
      (ignore-errors (es-put *es-index* *es-type* (cdr (assoc :id json)) polished-json-string)))))


(defun index-record (json-string &key (test #'identity) stream-position)
  (declare (ignore test))
  (let* ((json (ignore-errors (json:decode-json-from-string json-string))))
    (when (funcall test json)
      (when stream-position
        (print (list stream-position
                     (length json-string)
                     (cdr (assoc :title json))
                     (cdr (assoc :year json))
                     (cdr (assoc :keywords json))
                     (cdr (assoc :abstract json))
                     (cdr (assoc :fos json))
                     (cdr (assoc :id json))
                     )))))
  nil)


(defun save-to-es (json filename stream-position length)
  "Save complete MAG record into ElasticSearch."
  (declare (ignore filename stream-position length))
  (when (and json)
      (ignore-errors (es-put *es-index* *es-type* (cdr (assoc :id json)) json-string)))
  nil)




;;(unless (and *organizations-db* *venue-db* *authors-db*)
;;  (reset-mag))


(defun load-mag ()
  (let ((semaphore (bt-semaphore:make-semaphore :count 1)))
    (with-open-file (output "F:/mag-index.txt"
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :external-format :utf8)
      (flet ((record-processor (json filename position length)
               "Format record as a string: ID file-index offset length year number-of-urls title double-semicolon-separated-keywords abstract"
               ;; UTF-16 magic eliminates strange UTF-8 encoding error, see:
               ;; https://stackoverflow.com/questions/17665322/sbcl-encoding-error-only-when-executed-from-prompt
               (babel:octets-to-string
                (babel:string-to-octets
                 (format nil "~{~A~^ ~}"
                         (list (cdr (assoc :id json))
                               (let ((name (pathname-name filename)))
                                 (subseq name (1+ (position #\_ name :from-end t))))
                               position
                               length
                               (cdr (assoc :year json))
                               (length (cdr (assoc :url json)))
                               (cdr (assoc :title json))
                               (format nil "~@[ ;; ~{~A~^ ;; ~}~]"
                                       (or (cdr (assoc :keywords json)) nil))
                               (or (cdr (assoc :abstract json)) "")))
                 :encoding :utf-16le)
                :encoding :utf-16le))
             (filter (json)
               (and
                #+(or)(or
                 (null (cdr (assoc :fos json)))
                 (member "Medicine" (cdr (assoc :fos json)) :test #'string=)
                 (member "Biology" (cdr (assoc :fos json)) :test #'string=))
                (member (cdr (assoc :lang json)) '("en" "ru") :test #'string=)))
             (writer (data)
               (format output "~A~%" data))
             (writer-blocking (data)
               (bt-semaphore:wait-on-semaphore semaphore)
               (unwind-protect
                    (format output "~A~%" data)
                 (bt-semaphore:signal-semaphore semaphore))))
        (process-mag-files
         "g:/BibData/MAG/expanded"
         :pattern "mag_papers_0.txt"
         :record-filter #'filter
         :record-processor #'record-processor
         :output-writer #'writer
         :lines-limit nil)))))


(defun load-mag-record (file-name offset &optional length)
  (declare (ignore length))
  (with-open-file (mag-file file-name
                            :direction :input
                            :external-format :utf-8)
    (file-position mag-file offset)
    (json:decode-json-from-string (read-line mag-file nil nil))))




(defun save-dictionaries ()
  (flet ((dump-database (db pathname)
           (with-open-file
               (org-file pathname
                         :direction :output
                         :if-does-not-exist :create :if-exists :supersede)
             (loop for item being the hash-values of db
                do (format org-file "~4D: ~A~%"
                           (db-item-count item) (db-item-key item))))))
    (dump-database (names-database-items *organizations-db*)
                   #p"/users/serg/temp/mag-organizations.txt")
    (dump-database (names-database-items *venue-db*)
                   #p"/users/serg/temp/mag-venues.txt")
    (dump-database (names-database-items *authors-db*)
                   #p"/users/serg/temp/mag-authors.txt")))

;; (es-index-create *es-index* 1 0)

(defun generate-mag-id-to-urls ()
  (with-open-file (urls #p"mag-urls.py"
                        :direction :output
                        :external-format :utf-8
                        :if-does-not-exist :create :if-exists :supersede)
    (with-open-file (index #p"/space/1G/serg/MAG-enru-pharm-index.txt")
      (loop
         initially
           (format urls "def get_mag_urls():~%")
           (format urls "    urls = {}~%")
         for line = (read-line index nil nil)
         for line-number from 0
         while (and line) ; (< line-number 20))
         do
           (with-input-from-string (s line)
             (let ((id (read s nil))
                   (file-index (read s nil))
                   (offset (read s nil)))
               (declare (ignore id))
               (let* ((file-name (format nil "/space/1G/serg/expanded-aminer/mag/mag_papers_~D.txt" file-index))
                      (json (load-mag-record file-name offset nil)))
                 (format urls
                         "    urls['~A'] = [~{'~A'~^, ~}]~%"
                         (cdr (assoc :id json))
                         (cdr (assoc :url json))))))
         finally
           (format urls "    return urls~%")))))
