(in-package :saexplorer)

(defun get-home-directory ()
  cl-user::*project-home*)

(defun load-config (name &key (directory (get-home-directory)))
  (let ((filename (merge-pathnames name directory))
        (config (py-configparser:make-config)))
    (log-message :info "Loading config file ~A" filename)
    (setf saexplorer.sys::*config* (py-configparser:read-files config (list filename)))
    saexplorer.sys::*config*))

(defun get-option (config section-name option-name &optional default &key type)
  (handler-case
      (py-configparser:get-option config section-name option-name
                                  :type type
                                  :defaults `((,(cons option-name default))))
    (py-configparser:no-option-error () default)))


(defun setup-redis-cache ()
  (let ((host (get-option saexplorer.sys::*config* "Cache" "host" "localhost"))
        (port (get-option saexplorer.sys::*config* "Cache" "port" 6379 :type :number)))
    (redis:connect :host host :port port)))

(defun setup-cache ()
  (alexandria:switch ((get-option saexplorer.sys::*config* "Cache" "type") :test #'string-equal)
    ("redis" (setup-redis-cache))))

(defun connect-database ()
  "Open `dbi:connect' connection to the SQL database."
  (alexandria:eswitch ((get-option *config* "Database" "type") :test #'string-equal)
    ("sqlite3"
     (let* ((dbf (get-option *config* "Database" "name"))
            (relativep (char/= #\/ (aref dbf 0)))
            (home (get-home-directory))
            (complete-dbf (format nil "~A" (merge-pathnames
                                            (pathname dbf)
                                            (when relativep home)))))
       (setf *dbcon* (dbi:connect :sqlite3 :database-name complete-dbf))
       *dbcon*))))

(defun shutdown ()
  (dbi:disconnect *dbcon*)
  (redis:disconnect))


(defun main ()
  (setup-logging)
  (redis:connect)
  (log-message :info "Starting")
  (load-config #p"local.cfg")
  (log-message :info "Using cache database ~A" (get-option saexplorer.sys::*config* "Cache" "type"))
  (server:start-server :port (get-option saexplorer.sys::*config* "Server" "port" 8135 :type :number))
  (log-message :info "Visit http://localhost:8135/index.html"))
