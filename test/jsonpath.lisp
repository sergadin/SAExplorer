(in-package :saexplorer-test)

(defparameter *dict-json-str*
  "
{
  \"authors\": [
     {
       \"name\": \"Smith, J.\",
       \"id\": 123,
       \"affiliation\": {
          \"org\": \"Some university\",
          \"id\": \"org-17\"
       }
     },
     {
       \"name\": \"Bond, J.\",
       \"id\": 7
     }
  ],
  \"title\": \"Title of the work\",
  \"year\": 2018
}
")

(deftestsuite jsonpath (root)
  ((json (json:decode-json-from-string *dict-json-str*)))
  (:equality-test #'eql)
  (:documentation "."))


(addtest (jsonpath)
  jsonpath-valid-syntax
  (ensure-different (jsonpath.parser::parse "$.authors.id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[0].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[0:2].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[:2].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[2:].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[2:21:3].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[*].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[*]..id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[?(@.id > 0)].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[?(@.affiliation.id)].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[?(@.affiliation.id > $.year)].id") nil)
  (ensure-different (jsonpath.parser::parse "$.authors[?(@.affiliation[*].id > $..year)].id") nil)
  (ensure-different (jsonpath.parser::parse "$..*[?(@.affiliation..id > $.*[2]..year)].id") nil))


(addtest (jsonpath)
  jsonpath-invalid-syntax
  (ensure-error (jsonpath.parser::parse "$./authors.id"))
  (ensure-error (jsonpath.parser::parse "$.authors[1-2].id"))
  (ensure-error (jsonpath.parser::parse "$.authors[*]...id"))
  (ensure-error (jsonpath.parser::parse "authors[*].id"))
  (ensure-error (jsonpath.parser::parse "$.authors[].id"))
  (ensure-error (jsonpath.parser::parse "$.authors[?(@.id^1 > 0)].id"))
  (ensure-error (jsonpath.parser::parse "$.authors(?a).id")))

(defun set-equal (a b &key (test #'equal))
  (flet ((subset (a b)
           (loop for x in a
              when (not (member x b :test test)) do (return nil)
              finally (return t))))
    (and (subset a b) (subset b a))))

(addtest (jsonpath)
  jsonpath-results
  (ensure-same (jsonpath:process json "$..id") '(123 "org-17" 7) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors.id") '(123 7) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[0].id") '(123) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[:0].id") '(123) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[1]") '(((:name . "Bond, J.") (:id . 7))) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[1].id") '(7) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[*].id") '(123 7) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[0:].id") '(123 7) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[0:3].id") '(123 7) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[0:3:2].id") '(123) :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[12345].id") '() :test #'set-equal)
  (ensure-same (jsonpath:process json "$..affiliation.id") '("org-17") :test #'set-equal)
  (ensure-same (jsonpath:process json "$..affiliation..id") '("org-17") :test #'set-equal)
  (ensure-same (jsonpath:process json "$..affiliation.*") '("org-17" "Some university") :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[*].affiliation.id") '("org-17") :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[1].affiliation.id") '() :test #'set-equal)
  (ensure-same (jsonpath:process json "$..affiliation[?(@..id)]..id") '("org-17") :test #'set-equal)
  (ensure-same (jsonpath:process json "$..*[?(@.org)]..id") '("org-17") :test #'set-equal)
  (ensure-same (jsonpath:process json "$.authors[?(@..org)].id") '(123) :test #'set-equal :report "Deep filtering.")
  (ensure-same (jsonpath:process json "$.*[?(@..org)].id") '(123) :test #'set-equal :report "Deep filtering for wildcard node.")
  (ensure-same (jsonpath:process json "$..*[?(@..org)].id") '(123 "org-17") :test #'set-equal :report "Deep filtering for deep wildcard node."))

(addtest (jsonpath)
  jsonpath-match
  (ensure-same (jsonpath:match json "$.authors[1].id") 7 :test #'=))

;; More involved expressions
(addtest (jsonpath)
  jsonpath-results-relations
  (ensure-same (jsonpath:process json "$..authors[?(@.id < 20)].id")
               '(7)
               :test #'set-equal :report "Deep filtering for deep wildcard node.")
  (ensure-same (jsonpath:process json "$..authors[?(@..org = 'Some university')].id")
               '(123)
               :test #'set-equal :report "Strings comparision in filtering expression.")
  (ensure-same (jsonpath:process json "$..authors[?(@.id = '123')]..org")
               '("Some university")
               :test #'set-equal :report "Strings comparision in filtering expression."))
