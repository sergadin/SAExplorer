;;; Function for quering Web of Science Search API.

(in-package :cl-user)

(defpackage saexplorer.wos
  (:nicknames :wos)
  (:use :cl)
  (:import-from :cl-log
                #:log-message))

(in-package :saexplorer.wos)

(defparameter +auth-endpoint+ "http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate")
(defparameter +search-endpoint+ "http://search.webofknowledge.com/esti/wokmws/ws/WokSearch")

(defparameter +endpoint-host+ "localhost:8003")

(defun make-endpoint-url (service)
  (cond
    ((eq service :authenticate)
     (format nil "http://~A/esti/wokmws/ws/WOKMWSAuthenticate" +endpoint-host+))
    ((eq service :search)
     (format nil "http://~A/esti/wokmws/ws/WokSearchLite" +endpoint-host+))
    ((eq service :premium-search)
     (format nil "http://~A/esti/wokmws/ws/WokSearch" +endpoint-host+))))



(defun create-body-for-authenticate (&key close-session)
  (format nil "~{~A~%~}"
          (list "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\""
                "                  xmlns:auth=\"http://auth.cxf.wokmws.thomsonreuters.com\">"
                "<soapenv:Header/>"
                "<soapenv:Body>"
                (if close-session
                    "<auth:closeSession/>"
                    "<auth:authenticate/>")
                "</soapenv:Body>"
                "</soapenv:Envelope>")))


;; preparing the xml soap


    ;; AD=Address
    ;; AI=Author Identifiers (ResearcherID and ORCID)
    ;; AU=Author
    ;; CF=Conference
    ;; CI=City
    ;; CU=Country
    ;; DO=DOI
    ;; ED=Editor
    ;; FG=Grant Number
    ;; FO=Funding Agency
    ;; FT=Funding Text
    ;; GP=Group Author
    ;; IS=ISSN/ISBN
    ;; OG=Organization - Enhanced
    ;; OO=Organization
    ;; PS=Province/State
    ;; PY=Year Published
    ;; SA=Street Address
    ;; SG=Suborganization
    ;; SO=Publication Name
    ;; SU=Research Area
    ;; TI=Title
    ;; TS=Topic
    ;; UT=Accession Number
    ;; WC=Web of Science Category
    ;; ZP=Zip/Postal Code


(defun create-body-for-search-request-soap ()
  (format nil "~{~A~%~}"
          (list
           "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
           "
            <soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"
               xmlns:woksearchlite=\"http://woksearchlite.v3.wokmws.thomsonreuters.com\">
               <soapenv:Header/>
               <soapenv:Body>
                  <woksearchlite:search>
                     <queryParameters>
                        <databaseId>WOS</databaseId>
                        <userQuery>TS=(cadmium OR lead)</userQuery>
                        <editions>
                           <collection>WOS</collection>
                           <edition>SCI</edition>
                        </editions>
                        <timeSpan>
                           <begin>2000-01-01</begin>
                           <end>2011-12-31</end>
                        </timeSpan>
                        <queryLanguage>en</queryLanguage>
                     </queryParameters>
                     <retrieveParameters>
                        <firstRecord>1</firstRecord>
                        <count>5</count>
                     </retrieveParameters>
                  </woksearchlite:search>
               </soapenv:Body>
            </soapenv:Envelope>
           ")))

(defun create-body-for-premium-search-request-soap ()
  (format nil "~{~A~%~}"
          (list
           "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
           "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" "
           "                  xmlns:woksearch=\"http://woksearch.v3.wokmws.thomsonreuters.com\">"
           "<soapenv:Header/>"
           "<soapenv:Body>"
           "  <woksearch:search>"
           "    <queryParameters>"
           "      <databaseId>WOS</databaseId>"
           "      <userQuery>TS=(cadmium OR lead) AND PY=(2007 OR 2008)</userQuery>"
           "      <editions>"
           "        <collection>WOS</collection>"
           "        <edition>SCI</edition>"
           "      </editions>"
           "      <timeSpan>"
           "        <begin>2000-01-01</begin>"
           "        <end>2011-12-31</end>"
           "      </timeSpan>"
           "      <queryLanguage>en</queryLanguage>"
           "    </queryParameters>"
           "    <retrieveParameters>"
           "      <firstRecord>1</firstRecord>"
           "      <count>5</count>"
           "      <option>"
           "        <key>RecordIDs</key>"
           "        <value>On</value>"
           "      </option>"
           "      <option>"
           "        <key>targetNamespace</key>"
           "        <value>http://scientific.thomsonreuters.com/schema/wok5.4/public/FullRecord</value>"
           "      </option>"
           "    </retrieveParameters>"
           "  </woksearch:search>"
           "</soapenv:Body>"
           "</soapenv:Envelope>")))


(defun send-soap-request (&key url soap-action soap-body (protocol-version "1.1") cookie-jar auth-data)
  (let* ((auth-string (when auth-data
                        (destructuring-bind (login . password) auth-data
                          (format nil "Basic ~A" (base64:string-to-base64-string
                                                  (format nil "~A:~A" login password))))))
         (response
          (drakma:http-request url
                               :method :post
                               :content-length t
                               :content-type (if (string= protocol-version "1.1")
                                                 "text/xml; charset=utf-8"
                                                 "application/soap+xml; charset=utf-8") ;;for SOAP 1.2
                               :additional-headers  (mapcan #'(lambda (kv)
                                                                (when (cdr kv) (list kv)))
                                                            `(("SOAPAction" . ,soap-action)
                                                              ("Authorization" . ,auth-string)))
                               :cookie-jar cookie-jar
                               :content soap-body)))
    response))


(defun test ()
  (let* ((cookie-jar (make-instance 'drakma:cookie-jar))
         (auth (send-soap-request :url (make-endpoint-url :authenticate)
                                  :cookie-jar cookie-jar
                                  :soap-action "" ; "authenticate"
                                  :soap-body (create-body-for-authenticate)))
         (session-id (xpath:string-value
                      (xpath:evaluate "//return"
                                      (cxml:parse auth (stp:make-builder))))))
    (when (string= session-id "")
      (print auth))
    ;; Drop precise path assigned by Drakma
    (dolist (cookie (drakma:cookie-jar-cookies cookie-jar))
      (when (string-equal "SID" (drakma:cookie-name cookie))
        (setf (drakma:cookie-path cookie) "/")))
    (print session-id)
    (prog1
        (send-soap-request :url (make-endpoint-url :search)
                           :cookie-jar cookie-jar
                           :soap-action "" ;; search
                           :soap-body (create-body-for-search-request-soap))
      (send-soap-request :url (make-endpoint-url :authenticate)
                         :cookie-jar cookie-jar
                         :soap-action "" ; "closeSession"
                         :soap-body (create-body-for-authenticate :close-session t))
      nil)))
