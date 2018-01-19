;;;; Interface to Springer database.
;;;; For the list supported constraints see https://dev.springer.com/adding-constraints.

(in-package :cl-user)

(defpackage saexplorer.springer
  (:nicknames :springer)
  (:use :cl :saexplorer.sys :saexplorer.bibsystem)
  (:import-from :saexplorer
                #:normalize-keyword)
  (:import-from :cl-log
                #:log-message)
  (:import-from :py-configparser
                #:get-option))

(in-package :saexplorer.springer)

(defvar *sample-response* nil "A response used for offline debugging and testing.")

(defvar *facets-names*
  (list "keyword" "country" "year" "pub" "subject" "type")
  "The list of possible facet names supported by Springer.")


(define-bibsystem ("Springer")
    :accept-encodings '(:json "application/json"))


(defmethod bibsys::rest-endpoint ((system <springer>) query &key format)
  (declare (ignore query format))
  (format nil "~A/~A" (bibsys::config-option system "api-host") "metadata/json"))


(defmethod bibsys::rest-query-parameters ((system <springer>) query start chunk-size &key format)
  (declare (ignore format))
  `(("q" . ,query)
    ("p" . ,(format nil "~D" chunk-size))
    ("s" . ,(format nil "~D" start))
    ("api_key" . ,(bibsys::config-option system "api-key"))))

;;;
;;; Parsing responses
;;;

(defun extract-facet-values (springer-json facet-name)
  "Extract from SPRINGER-JSON the list of values of a facet named
FACET-NAME, as a list of plists (:name n :value v :count c),
compatible with `<facet>' items."
  (let ((facets (cdr (assoc :facets springer-json))))
    (flet ((facet-weighted-list (facet-name)
             (find-if #'(lambda (x) (string-equal facet-name (cdar x))) facets))
           (extract-values (facet-data)
             (mapcar #'(lambda (item)
                         (list
                          :name (cdr (assoc :value item))
                          :value (cdr (assoc :value item))
                          :count (parse-number:parse-number (cdr (assoc :count item)))))
                     (cdr (assoc :values facet-data)))))
      (extract-values (facet-weighted-list facet-name)))))

(defmethod bibsys::parse-response ((system <springer>) content format &key result-object)
  (declare (ignore system format))
  (let* ((springer-json (json:decode-json-from-string content))
         (total-results (parse-integer (cdr (assoc :total (cadr (assoc :result springer-json)))))) ;--- FIXME
         (result (or result-object (make-instance '<springer-result> :total-results total-results))))
    (with-accessors ((entries bibsys:entries)
                     (facets bibsys:facets))
        result
      (unless facets
        (setf facets (mapcar #'(lambda (facet-name)
                                 (make-instance '<facet>
                                                :name facet-name
                                                :items (extract-facet-values springer-json
                                                                             facet-name)))
                             *facets-names*)))
      (push (saexplorer::get-json-item springer-json '(:records)) entries))
    result))

;;;
;;; Query formatting
;;;

(defun make-query-for-keyword (keyword &key aliases subject)
  "Generates a query for Springer API."
  (format nil "(~{~A~^ OR ~})~@[ AND subject:\"~A\"~]"
          (mapcar #'(lambda (kw)
                      (concatenate 'string "keyword:\"" (normalize-keyword kw :safe-mode t) "\""))
                  (append (list keyword) aliases))
          subject))

;;;
;;; Data for offline debugging
;;;


(setf *sample-response*
'((:QUERY . "regular language")
 (:API-KEY . "31105d2b979c5bce288997f4a164ae80")
 (:RESULT ((:TOTAL . "109813") (:START . "1") (:PAGE-LENGTH . "1")))
 (:RECORDS ((:IDENTIFIER . "doi:10.1007/978-94-007-6883-3_12-1")
            (:URL ((:FORMAT . "") (:PLATFORM . "")
                   (:VALUE . "http://dx.doi.org/10.1007/978-94-007-6883-3_12-1")))
            (:TITLE . "Research in Related Disciplines and Non-Anglophone Areas")
            (:CREATORS ((:CREATOR . "Eemeren, Frans H."))
                       ((:CREATOR . "Garssen, Bart"))
                       ((:CREATOR . "Krabbe, Erik C. W."))
                       ((:CREATOR . "Snoeck Henkemans, A. Francisca"))
                       ((:CREATOR . "Verheij, Bart"))
                       ((:CREATOR . "Wagemans, Jean H. M.")))
            (:PUBLICATION-NAME . "Handbook of Argumentation Theory")
            (:OPENACCESS . "false")
            (:DOI . "10.1007/978-94-007-6883-3_12-1")
            (:PRINT-ISBN . "")
            (:ELECTRONIC-ISBN . "978-94-007-6883-3")
            (:ISBN . "")
            (:PUBLISHER . "Springer")
            (:PUBLICATION-DATE . "2021-01-01")
            (:ONLINE-DATE . "")
            (:PRINT-DATE . "")
            (:VOLUME . "")
            (:NUMBER . "")
            (:STARTING-PAGE . "")
            (:COPYRIGHT . "©2021 Springer Science+Business Media Dordrecht")
            (:GENRE . "ReviewPaper")
            (:ABSTRACT . "AbstractThis chapter discusses developments which have taken place, more or less independently, outside the research traditions treated in the earlier chapters. First, attention is paid to research in some disciplines and research programs that connect with argumentation theory and may even have some overlap with it. In Sect. 12.2 critical discourse analysis is discussed, in Sect. 12.3 historical controversy analysis, in Sect. 12.4 persuasion research and related quantitative research projects, and in Sect. 12.5 studies stemming from relevance theory which promote an argumentative turn in cognitive psychology.The next chapters concentrate on developments in argumentation research that have taken place in non-Anglophone parts of the world, in which research results are often published in other languages than English. Concentrating on contributions which have not yet been discussed in other chapters, in Sect. 12.6 an overview of argumentation research in the Nordic countries is given, in Sect. 12.7 of argumentation studies in German-speaking areas, and in Sect. 12.8 of argumentation studies in Dutch-speaking areas. The study of argumentation in French-speaking areas is discussed in Sect. 12.9, and the study of argumentation in Italian-speaking areas in Sect. 12.10.The next areas focused on are Eastern Europe, in Sect. 12.11, and Russia and other parts of the former USSR, in Sect. 12.12. Section 12.13 is devoted to the state of the art in argumentation theory in Spanish-speaking areas and Sect. 12.14 to the state of the art in Portuguese-speaking areas. Next, in Sect. 12.15 argumentation research in Israel is discussed, and in Sect. 12.16 argumentation research in the Arab world. The chapter concludes with an overview of the study of argumentation in Japan in Sect. 12.17 and an overview of the study of argumentation in China in Sect. 12.18.")))
 (:FACETS ((:NAME . "subject")
           (:VALUES ((:VALUE . "Computer Science") (:COUNT . "29704"))
                    ((:VALUE . "Medicine & Public Health") (:COUNT . "18091"))
                    ((:VALUE . "Artificial Intelligence (incl. Robotics)") (:COUNT . "12359"))
                    ((:VALUE . "Mathematical Logic and Formal Languages") (:COUNT . "8550"))
                    ((:VALUE . "Education") (:COUNT . "8259"))
                    ((:VALUE . "Public Health") (:COUNT . "7286"))
                    ((:VALUE . "Software Engineering") (:COUNT . "7162"))
                    ((:VALUE . "Logics and Meanings of Programs") (:COUNT . "7140"))
                    ((:VALUE . "Programming Languages, Compilers, Interpreters") (:COUNT . "6512"))
                    ((:VALUE . "Algorithm Analysis and Problem Complexity") (:COUNT . "5901"))
                    ((:VALUE . "Psychology") (:COUNT . "5562"))
                    ((:VALUE . "Life Sciences") (:COUNT . "5379"))
                    ((:VALUE . "Engineering") (:COUNT . "5184"))
                    ((:VALUE . "Computer Communication Networks") (:COUNT . "5156"))
                    ((:VALUE . "Theory of Computation") (:COUNT . "5155"))
                    ((:VALUE . "Computation by Abstract Devices") (:COUNT . "5147"))
                    ((:VALUE . "Social Sciences") (:COUNT . "5002"))
                    ((:VALUE . "Information Systems Applications (incl. Internet)") (:COUNT . "4960"))
                    ((:VALUE . "Programming Techniques") (:COUNT . "4842"))
                    ((:VALUE . "Software Engineering/Programming and Operating Systems") (:COUNT . "4640"))))
          ((:NAME . "pub")
           (:VALUES ((:VALUE . "BMC Public Health") (:COUNT . "1269"))
                    ((:VALUE . "BMC Health Services Research") (:COUNT . "650"))
                    ((:VALUE . "BMC Bioinformatics") (:COUNT . "528"))
                    ((:VALUE . "Knowledge and Information Systems") (:COUNT . "475"))
                    ((:VALUE . "Plant and Soil") (:COUNT . "475"))
                    ((:VALUE . "Trials") (:COUNT . "442"))
                    ((:VALUE . "Journal of Autism and Developmental Disorders") (:COUNT . "440"))
                    ((:VALUE . "Reading and Writing") (:COUNT . "420"))
                    ((:VALUE . "Developments in Language Theory") (:COUNT . "408"))
                    ((:VALUE . "The Journal of Supercomputing") (:COUNT . "399"))
                    ((:VALUE . "Automata, Languages and Programming") (:COUNT . "381"))
                    ((:VALUE . "Journal of Computer Science and Technology") (:COUNT . "375"))
                    ((:VALUE . "Journal of Immigrant and Minority Health") (:COUNT . "369"))
                    ((:VALUE . "Language and Automata Theory and Applications") (:COUNT . "357"))
                    ((:VALUE . "Software & Systems Modeling") (:COUNT . "336"))
                    ((:VALUE . "Algorithmic Learning Theory") (:COUNT . "332"))
                    ((:VALUE . "Implementation and Application of Automata") (:COUNT . "330"))
                    ((:VALUE . "BMC Psychiatry") (:COUNT . "317"))
                    ((:VALUE . "Computer Aided Verification") (:COUNT . "317"))
                    ((:VALUE . "AIDS and Behavior") (:COUNT . "310"))))
          ((:NAME . "year") (:VALUES ((:VALUE . "2021") (:COUNT . "34")) ((:VALUE . "2020") (:COUNT . "9")) ((:VALUE . "2018") (:COUNT . "899")) ((:VALUE . "2017") (:COUNT . "16779")) ((:VALUE . "2016") (:COUNT . "17304")) ((:VALUE . "2015") (:COUNT . "12392")) ((:VALUE . "2014") (:COUNT . "11705")) ((:VALUE . "2013") (:COUNT . "9453")) ((:VALUE . "2012") (:COUNT . "7366")) ((:VALUE . "2011") (:COUNT . "6292")) ((:VALUE . "2010") (:COUNT . "5152")) ((:VALUE . "2009") (:COUNT . "4226")) ((:VALUE . "2008") (:COUNT . "3351")) ((:VALUE . "2007") (:COUNT . "2293")) ((:VALUE . "2006") (:COUNT . "2208")) ((:VALUE . "2005") (:COUNT . "1440")) ((:VALUE . "2004") (:COUNT . "1251")) ((:VALUE . "2003") (:COUNT . "870")) ((:VALUE . "2002") (:COUNT . "676")) ((:VALUE . "2001") (:COUNT . "638"))))
          ((:NAME . "country") (:VALUES ((:VALUE . "United States") (:COUNT . "28349"))
                                        ((:VALUE . "United Kingdom") (:COUNT . "11489"))
                                        ((:VALUE . "Germany") (:COUNT . "10925"))
                                        ((:VALUE . "") (:COUNT . "9128"))
                                        ((:VALUE . "France") (:COUNT . "6679"))
                                        ((:VALUE . "Canada") (:COUNT . "6476"))
                                        ((:VALUE . "Australia") (:COUNT . "6159"))
                                        ((:VALUE . "Italy") (:COUNT . "4930"))
                                        ((:VALUE . "China") (:COUNT . "3744"))
                                        ((:VALUE . "Spain") (:COUNT . "3675"))
                                        ((:VALUE . "India") (:COUNT . "2667"))
                                        ((:VALUE . "Switzerland") (:COUNT . "2660"))
                                        ((:VALUE . "Japan") (:COUNT . "2622"))
                                        ((:VALUE . "Sweden") (:COUNT . "2443"))
                                        ((:VALUE . "Belgium") (:COUNT . "2062"))
                                        ((:VALUE . "Austria") (:COUNT . "1805"))
                                        ((:VALUE . "Poland") (:COUNT . "1646"))
                                        ((:VALUE . "Israel") (:COUNT . "1632"))
                                        ((:VALUE . "Brazil") (:COUNT . "1598"))
                                        ((:VALUE . "Norway") (:COUNT . "1492"))))
          ((:NAME . "type") (:VALUES ((:VALUE . "Book") (:COUNT . "58436"))
                                     ((:VALUE . "Journal") (:COUNT . "51377"))))
          ((:NAME . "keyword") (:VALUES ((:VALUE . "Children") (:COUNT . "564"))
                                        ((:VALUE . "Quality of life") (:COUNT . "441"))
                                        ((:VALUE . "Physical activity") (:COUNT . "416"))
                                        ((:VALUE . "Education") (:COUNT . "403"))
                                        ((:VALUE . "Systematic review") (:COUNT . "378"))
                                        ((:VALUE . "Depression") (:COUNT . "375"))
                                        ((:VALUE . "Autism") (:COUNT . "352"))
                                        ((:VALUE . "Gender") (:COUNT . "321"))
                                        ((:VALUE . "HIV") (:COUNT . "321"))
                                        ((:VALUE . "Adolescents") (:COUNT . "318"))
                                        ((:VALUE . "Assessment") (:COUNT . "313"))
                                        ((:VALUE . "Prevention") (:COUNT . "311"))
                                        ((:VALUE . "Intervention") (:COUNT . "303"))
                                        ((:VALUE . "Meta-analysis") (:COUNT . "280"))
                                        ((:VALUE . "Mental health") (:COUNT . "278"))
                                        ((:VALUE . "China") (:COUNT . "270"))
                                        ((:VALUE . "Qualitative research") (:COUNT . "258"))
                                        ((:VALUE . "Culture") (:COUNT . "254"))
                                        ((:VALUE . "Obesity") (:COUNT . "249"))
                                        ((:VALUE . "Communication") (:COUNT . "245"))))))

)
