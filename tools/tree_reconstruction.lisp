(ql:quickload 'cl-ppcre)

(require 'cl-ppcre)

(defparameter *number-of-lines-to-process* (* 200 1000 1000))
(defparameter *heartbeat-every-n-lines* (* 1000 1000))

(defvar *fos-relations* (make-hash-table) "Two-levels hash table. It's members are hash tables mapping fos-id to counts.")

(defvar *fos-counter* 0 "Number of FOS labels in *fos-labels*.")
(defvar *fos-labels* (make-hash-table :test #'equal))
(defvar *fos-id-to-label* (make-hash-table :test #'eql) "Reverse mapping to `*fos-labels*'.")

(defvar *fos-frequency* (make-hash-table) "Frequences of fos in the input file.")


(defun top-n (seq n)
  (loop for item in seq for i from 1 to n collect item))


(defun lookup-fos (fos-label &key (auto-extend t) preferred-id)
  "Convers fos label into its ID. If auto-extend is T, then missing
labels are inserted into the mapping with given PREFERRED-ID or
automatically selected id."
  (let ((id (gethash fos-label *fos-labels*)))
    (when (and (null id) auto-extend)
      (setf id (or preferred-id (incf *fos-counter*)))
      (setf (gethash fos-label *fos-labels*) id))
    id))

(defun fos-label<-id (fos-id)
  (gethash fos-id *fos-id-to-label*))


(defun parse-line (line)
  "Convert python-list of strings into ids."
  ;; [u'Intensive Care Medicine', u'Diabetes mellitus']
  ;; [u"Simpson's rule",]
  (when line
    (mapcar #'(lambda (fos-label)
                ;; Drop opening/closing quote '/" and unicode marker before searching for fos label
                (lookup-fos (cl-ppcre:regex-replace-all "(^\\s*\\[u['\"])|(['\"]\\]?\\s*.?$)" fos-label "")))
            ;; split by ', u'  OR ", u" OR mixed combination of ' and "
            (cl-ppcre:split "['\"], u['\"]"
                            line))))


(declaim (inline register-related-fos-id))
(defun register-related-fos-id (fos-id related-fos-id &optional count)
  "Add related-fos-id into hash table associated with fos-id in `*fos-relations*'.
   If COUNT is not specified when increment the counter, otherwise use the value provided."
  (let ((fos-relations-ht (gethash fos-id *fos-relations*)))
    ;; Create hash table for fos-1 relations of needed.
    (when (null fos-relations-ht)
      (setf fos-relations-ht (make-hash-table))
      (setf (gethash fos-id *fos-relations*) fos-relations-ht))
    (if count
        (setf (gethash related-fos-id fos-relations-ht) count)
        (incf (gethash related-fos-id fos-relations-ht 0)))))


(defun process (filename)
  "Process entire file with fos tuples."
  (with-open-file
      (f filename :direction :input)
    (loop :for line = (read-line f nil nil)
       :for foses = (parse-line line)
       :for line-number :from 1
       :while (and line (<= line-number *number-of-lines-to-process*))
       :when foses
       :do
       ;; (print line)
       ;; (print foses)
         (when (= (mod line-number *heartbeat-every-n-lines*) 0)
           (print line-number))
         (do* ((first-items foses (cdr first-items))
               (fos-1 (car first-items) (car first-items)))
              ((null first-items))
           ;; Count first fos occurrence
           (incf (gethash fos-1 *fos-frequency* 0))
           ;; Count all fos pairs
           (dolist (fos-2 (cdr first-items))
             ;; Count second fos occurrence
             (incf (gethash fos-2 *fos-frequency* 0))
             ;; Count symmetric co-occurrence
             (register-related-fos-id fos-1 fos-2)
             (register-related-fos-id fos-2 fos-1))))))


(defstruct %parent
  "Description of possble parent of a fos."
  fos-id
  frequency)

(defun is-a-child (fos-id generalization-chain)
  "Check that FOS-ID is a possible child of GENERALIZATION-CHAIN.

The chain is given in reverse order, starting from most specific foses."
  (let* ((chain (sort (copy-seq generalization-chain) #'< :key #'%parent-fos-id))
         (most-specific-generalization (%parent-fos-id (first chain))))
    (if (eql (find-parent fos-id :most-frequent t) most-specific-generalization)
        t
        (multiple-value-bind (parent-id parents-chain)
            (find-parent fos-id :most-frequent nil)
          (or (eql parent-id most-specific-generalization)
              (every #'(lambda (p)
                         (member p generalization-chain :key #'%parent-fos-id))
                     parents-chain))))))
  ;;(eql (find-parent fos-id :most-frequent t) (%parent-fos-id (first generalization-chain))))


(defun find-parent (fos-id &key most-frequent)
  "Find supposed parent for the specified FOS-ID. If most-trequent is T,
then the most frequent fos with larger frequency returned."
  (unless (gethash fos-id *fos-relations*)
    (return-from find-parent nil))
  (let (parents
        (related-ht (gethash fos-id *fos-relations*))
        related-fos-ids
        (fos-frequency (gethash fos-id *fos-frequency* 0))
        ;; How many candidates can violate generalization chain conditions
        ;; before search termination.
        (candidates-tolerance 5))
    ;; Order all related foses by their co-occurrence count, starting from most frequent ones.
    (setf related-fos-ids
          (sort
           (loop for related-fos-id being each hash-key of related-ht
              collect related-fos-id)
           #'>
           :key #'(lambda (fid) (gethash fid related-ht))))
    ;; Find a "closest" generalization that co-occurs most frequently.
    (loop :with parents-found = 0
       :for related-fos-id in related-fos-ids :and candidates-checked from 0
       :while (<= (- candidates-checked parents-found) candidates-tolerance)
       :do
         (let ((related-fos-frequency (gethash related-fos-id *fos-frequency* 0)))
           #+(or)
           (unless most-frequent (format t "Checking ~A (~A)~%"
                                         (gethash related-fos-id *fos-id-to-label*)
                                         related-fos-frequency))
           (when (and (> related-fos-frequency fos-frequency) ; is a generalization for fos-id, and
                      (or (zerop parents-found) ; first found generalization
                          ;; member of a generalization chain
                          (and (not most-frequent) ; deeper search is required
                               parents ; previous candidate exists
                               (is-a-child related-fos-id parents))))
             (push (make-%parent :fos-id related-fos-id :frequency related-fos-frequency) parents)
             (incf parents-found))))
    (setf parents (sort parents #'< :key #'%parent-frequency))
    (if parents
        (values (%parent-fos-id (first parents))
                (reverse (cons fos-id (mapcar #'%parent-fos-id parents))))
        (values nil nil))))

(defvar *top-categories* "List of conses (fos-id . count).")
(defun find-root-categories ()
  "Update *TOP-CATEGORIES* with "
  (let ((topness-counts-ht (make-hash-table)))
    (loop for fos-id being each hash-values of *fos-labels*
       for parent-id = (find-parent fos-id :most-frequent t)
       when parent-id
       do (incf (gethash parent-id topness-counts-ht 0)))

    (setf *top-categories*
          (sort (loop for top-id being each hash-key of topness-counts-ht using (hash-value count)
                   collect (cons top-id count))
                #'>
                :key #'cdr))
    t))

(defun show-top-categories ()
  (loop
     with roots = (remove-if #'(lambda (fid) (find-parent fid :most-frequent t))
                             *top-categories*
                             :key #'car)
     for (fos-id . topness-count) in roots and seq from 0
     while (< seq 50)
     do
       (format t "~5D votes for ~A (~D) [~~~DM]~%"
               topness-count
               (gethash fos-id *fos-id-to-label*)
               fos-id
               (/ (floor (* 10 (gethash fos-id *fos-frequency*)) 1000000) 10.0))))


;; ---FIXME: find-parent no longer prints to output string
(defun parents-for-all (filename)
  (with-open-file (f filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-string
     (with-output-to-string (*standard-output*)
       (loop for fos-id being each hash-values of *fos-labels*
          do (find-parent fos-id)))
     f)))



(defun dump-stat (filename)
  "Write computed statistics into file spacified by FILENAME."
  (format t "Dumping results~%")
  (let ((fos-id-to-label (make-hash-table)))
    ;; Construct fos-id to label mapping
    (loop for fos-id being each hash-values of *fos-labels* using (hash-key fos-label)
       do (setf (gethash fos-id fos-id-to-label) fos-label))

    (with-open-file (stats-file filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      ;; Write down fos dictionary with frequences
      (format stats-file "# FOS_ID   COUNT PARENT_FOS_ID  FOS_NAME <-- PARENT_NAME~%")
      (loop for frequency being each hash-values of *fos-frequency* using (hash-key fos-id)
         for parent-fos-id = (find-parent fos-id)
         do (format stats-file "~5D ~9D ~9A ~A <-- ~A~%"
                    fos-id
                    frequency
                    parent-fos-id
                    (gethash fos-id fos-id-to-label "*** UNDEFINED ID ***")
                    (gethash parent-fos-id fos-id-to-label)))
      ;; Write down co occurrence matrix
      (format stats-file "#~%# FOS_1_ID FOS_2_ID COUNT~%#~%")
      (loop for related-ht being each hash-values of *fos-relations* using (hash-key fos-1-id)
         do
           (loop for count being each hash-values of related-ht using (hash-key fos-2-id)
              for fos-1-label = (gethash fos-1-id fos-id-to-label)
              and fos-2-label = (gethash fos-2-id fos-id-to-label)
              :do
              ;; format ignores tow positional parameters - the labels
                (format stats-file "~5D ~5D ~9D ~2*~%" fos-1-id fos-2-id count fos-1-label fos-2-label))))))


(defun load-stat-file (filename)
  "Restore data from file created by `dump-stat':
     fos dictionary, fos frequencies, fos co-occurrence statistics."
  (with-open-file
      (f filename :direction :input)
    (loop :for line = (read-line f nil nil)
       :for line-number :from 1
       :with file-section = :none
       :while (and line)
       :do
         (when (eql #\# (elt line 0))
           (ecase file-section
             ((:none) (setf file-section :dictionary))
             ((:dictionary) (setf file-section :co-occurrence-stat))
             ((:co-occurrence-stat) nil)))
         (case file-section
           ((:dictionary)
            ;; Process fos description line of the form:
            ;;     FOS_ID:int COUNT:int PARENT_FOS_ID:int  FOS_NAME:str <-- PARENT:str
            (ppcre:register-groups-bind
                (fos-id-str count-str parent-fos-id-str fos-name parent-name)
                ("^\\s*(\\d+)\\s+(\\d+)\\s+(NIL|\\d+)\\s+(.+) <-- (.+)" line)
              (declare (ignore parent-name))
              (let ((fos-id (parse-integer fos-id-str))
                    (count (parse-integer count-str))
                    (parent-fos-id (parse-integer parent-fos-id-str :junk-allowed t)))
                (declare (ignore parent-fos-id))
                (lookup-fos fos-name :preferred-id fos-id)
                (setf (gethash fos-id *fos-id-to-label*) fos-name)
                (setf (gethash fos-id *fos-frequency*) count))))
           ((:co-occurrence-stat)
            ;; Process fos co occurrence statistics line of the form:
            ;;    FOS-1:int FOS-2:int COUNT:int
            (ppcre:register-groups-bind
                (fos-1-id fos-2-id count)
                ("\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)" line)
              (register-related-fos-id (parse-integer fos-1-id)
                                       (parse-integer fos-2-id)
                                       (parse-integer count)))))))
  t)


(defun fos-profile (fos-name &key (top-n 20))
  "Display information about fos given by its name."
  (let* ((fos-id (lookup-fos fos-name :auto-extend nil))
         (fos-frequency (gethash fos-id *fos-frequency* 0))
         (relations-ht (gethash fos-id *fos-relations*))
         related-foses)
    (when relations-ht
      (setf related-foses
            (loop for count being each hash-values of relations-ht using (hash-key fos-2-id)
               collect (cons fos-2-id count)))
      ;; FOS description
      (format t "~A (~D) appears ~D times.~%" fos-name fos-id (gethash fos-id *fos-frequency*))
      ;; Print parents chain
      (multiple-value-bind (parent-id parents)
          (find-parent fos-id)
        (format t "Parents:~%")
        (format t "~{~A (~D)~^ / ~}~%" (mapcan #'(lambda (p)
                                                   (list (gethash p *fos-id-to-label*) p))
                                               parents)))
      (flet ((show-top-records (n related-foses &optional message)
               (when message
                 (format t "~&~A~%" message)
               (loop
                  for (fos-2-id . count) in related-foses and line-number from 0
                  for fos-2-name = (gethash fos-2-id *fos-id-to-label*)
                  and fos-2-frequency = (gethash fos-2-id *fos-frequency* 0)
                  while (< line-number n)
                  do
                    (format t "~5D ~5D ~9D ~A (~D) --- ~A (~D)~%"
                            fos-id fos-2-id count
                            fos-name fos-frequency
                            fos-2-name fos-2-frequency))))
             (g-freq (record) ; global frequency of record's fos
               (gethash (car record) *fos-frequency* 0))
             (co-occ-freq (record) (cdr record)))
        (show-top-records top-n (sort related-foses #'> :key #'cdr) "By co occurrence frequency")
        (show-top-records (min top-n 10)
                          (sort (top-n (sort related-foses #'> :key #'g-freq) (* 6 top-n))
                                #'>
                                :key #'co-occ-freq)
                          "Top 2n by global frequency sorted by co occurrence")))))


;; /home/stp/data/fos.txt
;; /Users/serg/fos.txt
(defun main (&key (mode :discover-parents))
  (let (data-file-name stats-file-name production)
    (if (probe-file #P"/home/stp/data/fos.txt")
        (setf production t
              data-file-name #P"/home/stp/data/fos.txt"
              stats-file-name #P"fos-pairs-stats.txt")
        ;; debug settings
        (setf data-file-name #P"/Users/serg/fos.txt"
              stats-file-name #P"/Users/serg/fos-pairs-stats.txt"))
    (ecase mode
      ((:collect-statistics)
       (process data-file-name)
       (dump-stat stats-file-name))
      ((:discover-parents)
       (load-stat-file stats-file-name)))
    (fos-profile "Mathematics")
    (when production
      #+or(quit))))
