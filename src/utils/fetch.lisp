(in-package :saexplorer.utils)


(defconstant +http-ok+ 200)

(defconstant +default-proxy-port+ 3128)


(defun fetch-url (url &key use-proxy)
  "Return content located at the specified URL."
  (log-message :trace "Downloading: ~A" url)
  (let ((proxy
         (when use-proxy
           (list (py-configparser:get-option *config* "Proxy" "host")
                 (or (py-configparser:get-option *config* "Proxy" "port" :type :number)
                     +default-proxy-port+))))
        (proxy-auth
         (when use-proxy
           (list (py-configparser:get-option *config* "Proxy" "username")
                 (py-configparser:get-option *config* "Proxy" "password")))))
    (multiple-value-bind (content status-code server-headers)
        (drakma:http-request url
                             :method :get
                             :proxy proxy
                             :proxy-basic-authorization proxy-auth
                             :external-format-in :utf-8
                             :external-format-out :utf-8)
      (when (= status-code +http-ok+)
        ;; Decode content into string, if needed
        (multiple-value-bind (type subtype params)
            (drakma:get-content-type server-headers)
          (if (and (member type '("text") :test #'string-equal)
                   (member subtype '("plain" "html" "xml") :test #'string-equal))
              content
              (handler-case
                  (progn
                    (log-message :debug "Decoding from content-type: ~A/~A; ~A" type subtype params)
                    (flexi-streams:octets-to-string content :external-format :utf-8))
                (flexi-streams:external-format-encoding-error (e)
                  ;; Some engines do not encode content properly?
                  (log-message :error "Content decoding failed: ~A" e)
                  content))))))))
