(in-package :saexplorer-test)

(deftestsuite conference-names (root)
  ()
  (:equality-test #'string-equal)
  (:documentation
   "Tests for ability to extract and predict conferences names from
   their proceedings titles."))


(addtest (conference-names)
  without-abbreviations
  (ensure-null (confs::guess-abbreviation
                "International Conference On Control Automation And Systems")))

(addtest (conference-names)
  simple-abbreviations
  (ensure-same (confs::guess-abbreviation
                "Proceedings 15th International Parallel And Distributed Processing Symposium IPDPS 2001")
               (values "IPDPS" "International Parallel And Distributed Processing Symposium"))
  (ensure-same (confs::guess-abbreviation
                "7th International Workshop On Descriptional Complexity Of Formal Systems Dcfs 2005 Proceedings")
               (values "DCFS" "Descriptional Complexity Of Formal Systems")))

(addtest (conference-names)
  short-abbreviations
  (ensure-same (confs::guess-abbreviation
                "Proceedings Of The Prague Stringology Conference 2013 Psc 2013")
               (values "PSC" "Prague Stringology Conference"))
  (ensure-same (confs::guess-abbreviation
                "Lecture Notes In Informatics Lni Proceedings Series Of The Gesellschaft Fur Informatik Gi")
               (values "LNI" "Lecture Notes In Informatics")
               :report "Abbreviation expansion ends with a preposition.")
  t)

;; Not implemented
#+(or)(addtest (conference-names)
  extended-abbreviations
  (ensure-same (confs::guess-abbreviation
                "Proceedings 2005 IEEE International Conference On E Technology E Commerce And E Service Eee 05")
               (values "EEE" "E Technology E Commerce And E Service")
               :report "Extend conference full name"))


;; Itat 2006 Workshop On Theory And Practice Of Information Technologies Applications And Theory Proceedings
;;
;; Naacl Hlt 2012 2012 Conference Of The North American Chapter Of The Association For Computational Linguistics Human Language Technologies Proceedings Of The Conference

;; Principles Of Knowledge Representation And Reasoning Proceedings Of The 15th International Conference Kr 2016
