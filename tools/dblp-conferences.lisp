;;;;
;;;; Populate SQL database with conference titles from DBLP.org.
;;;;
;;;; Main function is `update-dblp-conferences'.
;;;;

(in-package :saexplorer.tools)

(defun extract-venue (hit-item)
  "Extract conference description from parsed JSON element coresponding to a hit item of DBLP response."
  (mapcan #'(lambda (x) (destructuring-bind (key path) x
                          (list key (jsonpath:match hit-item path))))
          '((:id "$.@id")
            (:name "$.info.venue")
            (:acronym "$.info.acronym")
            (:type "$.info.type")
            (:key "$.info.url"))))

(defun get-all-venues (term-prefix &key (max-results nil))
  "Return descriptions of all conferences containing a word starting with TERM-PREFIX in the title."
  (let ((chunk-size 100)
        (total-results nil)
        venues)
    (do ((start 0 (incf start chunk-size)))
        ((or (< 0 (or total-results 1) start) ; second iteration, no TOTAL-RESULTS
             (and total-results (>= start (min total-results (or max-results total-results))))))
      (cl-log:log-message :trace "Querying DBLP. start=~D, total=~D." start total-results)
      (multiple-value-bind (content status-code)
          (drakma:http-request "http://dblp.org/search/venue/api"
                               :external-format-out :utf-8
                               :parameters (list (cons "q" term-prefix)
                                                 (cons "h" (format nil "~D" chunk-size))
                                                 (cons "f" (format nil "~D" start))
                                                 (cons "format" "json"))
                               :accept "application/json")
        (when (= status-code 200)
          (let* ((chunk-content (flexi-streams:octets-to-string content :external-format :utf-8))
                 (document (json:decode-json-from-string chunk-content))
                 (hits (jsonpath:match document "$.result.hits")))
            (unless total-results
              (setf total-results (parse-integer (jsonpath:match hits "$.@total"))))
            (push (mapcar #'extract-venue (jsonpath:match hits "$.hit"))
                  venues)))))
    (reduce #'append venues :initial-value '())))


;;--- FIXME: move this function to .models
(defun create-db-schema (&key (dry t))
  (loop with classes = '(<bibdb> <entity> <conf-type> <conf-info>)
     for model in classes
     do
       (mito:ensure-table-exists model)
       (if dry
           (print (mito:migration-expressions model))
           (mito:migrate-table model)))
  t)


(defun store-conference (conf-description &key bibdb)
  "Store information into SQL database."
  (cl-log:log-message :trace "Storing ~A: ~A"
                      (or (getf conf-description :acronym)
                          (getf conf-description :name))
                      (getf conf-description :key))
  (let* ((typename (getf conf-description :type))
         (conftype (or (mito:find-dao '<conf-type> :name typename)
                       (mito:create-dao '<conf-type>
                                        :name typename :code (string (subseq typename 0 4))))))
    (mito:create-dao '<conf-info>
                     :name (getf conf-description :name)
                     :acronym (getf conf-description :acronym)
                     :conftype conftype
                     :entity (mito:create-dao '<entity>
                                              :bibdb bibdb
                                              :category models:+entity-cat-conference+
                                              :identifier (getf conf-description :id)))))

(defun delete-conferences-for (bibdb dbconn)
  "Delete all conferences originated from `<BIBDB>'."
  (let* ((ent-subquery-sql "SELECT id
            FROM entity JOIN bibdb ON (bibdb.id = entity.bibdb_id)
            WHERE bibdb.id = ? AND enity.category = ? ")
         (del-confs-sql (format nil "~{~A ~}"
                                `("DELETE FROM conference WHERE entity_id IN ("
                                  ,ent-subquery-sql
                                  ")")))
         (del-ent-sql (format nil "DELETE FROM entity WHERE id IN (~A)" ent-subquery-sql)))
    (let ((query (dbi:prepare dbconn del-confs-sql)))
      (dbi:execute query (mito:object-id bibdb) models:+entity-cat-conference+))
    (let ((query (dbi:prepare dbconn del-ent-sql)))
      (dbi:execute query (mito:object-id bibdb) models:+entity-cat-conference+)))
  t)


(defun update-dblp-conferences (&key clear-old)
  "Download all conferences from DBLP server without caching. If
CLEAR-OLD is non-NIL all records are removed."
  (let* ((mito:*connection* saexplorer.sys::*dbcon*)
         (dblp (mito:find-dao '<bibdb> :code "DBLP")))
    (create-db-schema :dry nil) ;--- FIXME: check migration at startup
    (when (and clear-old dblp)
      ;; Remove old records
      (delete-conferences-for bibdb saexplorer.sys::*dbcon*))
    ;; Download, sort and dedup records
    (loop with bibdb = (or dblp (mito:create-dao '<bibdb> :code "DBLP" :name "DBLP.org"))
       for c-descr in (sort (loop for ch across "abcdefghijklmnopqrstuvwxzy"
                               nconcing (get-all-venues (string ch)))
                            #'string-lessp :key #'(lambda (x) (getf x :name)))
       for last-id = nil then id
       for id = (getf c-descr :id)
       do
       (when (not (equal id last-id))
         (handler-case
             (store-conference c-descr :bibdb bibdb)
           (dbi:<dbi-error> (e)
             (cl-log:log-message :error "~A" e)))))))
