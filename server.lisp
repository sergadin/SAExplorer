(ql:quickload :saexplorer)
(ql:quickload :yason)
(ql:quickload :woo)
(ql:quickload :clack)


(saexplorer:init)
(mito:ensure-table-exists 'saexplorer.models::<cfp-page>)
(mito:migrate-table 'saexplorer.models::<cfp-page>)
;;(saexplorer.cfp:collect "wikicfp")
;;(saexplorer.cfp:collect "mathmeetings")
;;(saexplorer.cfp:collect "ams")
;;(saexplorer.cfp:collect)

(defun encode (data)
    (json:encode-json-plist-to-string `("data" ,data)))


(defun get-cfps ()
  (loop for cfp in (mito:retrieve-dao 'saexplorer.models::<cfp-page>)
                    collect (alexandria:plist-hash-table
                             `("quartile" ,(1+ (random 4))
                               "year" 2018
                               "dates" ,(saexplorer.models::cfp-dates cfp)
                               "name" ,(saexplorer.models::cfp-name cfp)
                               "url" ,(saexplorer.models::cfp-url cfp)
                               "location" ,(saexplorer.models::cfp-location cfp)
                               "deadline" "Deadline"))))

;; (print (encode (get-cfps)))

(clack:clackup
   (lambda (env)
     (declare (ignore env))
     (list 200 '(:content-type "application/json"
                 :Access-Control-Allow-Origin "*") ;;"http://scientific-events.info")
           (list (encode (get-cfps)))))
   :server :woo
   :port 8100
   :use-default-middlewares nil)
