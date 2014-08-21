(in-package :cl-user)
(defpackage :corvus-test.parser
  (:use :cl :fiveam :corvus.ast :corvus-test.util)
  (:import-from :corvus.parser
                :parse-string))
(in-package :corvus-test.parser)

(def-suite parser)
(in-suite parser)

(test integer
  (is-type (parse-string "1") '<integer>)
  (is-type (parse-string "1234") '<integer>)
  (is-type (parse-string "+213213") '<integer>)
  (is-type (parse-string "-23232") '<integer>))

(test float
  (is-type (parse-string "23.0") '<float>)
  (is-type (parse-string "3.14") '<float>)
  (is-type (parse-string "+3.14") '<float>)
  (is-type (parse-string "-3.14") '<float>)
  (is-type (parse-string "45e10") '<float>)
  (is-type (parse-string "-10e2") '<float>))

(test string
  (is-type (parse-string "\"\"") '<string>)
  (is-type (parse-string "\"test\"") '<string>)
  (is (equal (val (parse-string "\"test\""))
             "test")))

(test identifier
  (is-type (parse-string "test") '<identifier>)
  (is-type (parse-string "a123") '<identifier>)
  (is-type (parse-string "@test") '<identifier>)
  (is-type (parse-string "3test3") '<identifier>)
  (is-type (parse-string "test-test-test") '<identifier>))

(test list
  (is-true (null (parse-string "()")))
  (is-type (parse-string "(1 2 3)") 'list)
  (is (equal (length (parse-string "(1 2 3)"))
             3))
  (is (equal (tree-to-string (parse-string "(1 2 3)"))
             (list "1" "2" "3")))
  (is (equal (tree-to-string (parse-string "(if cond (f 1) (g 2))"))
             (list "if" "cond" (list "f" "1") (list "g" "2")))))

(test array
  (is (equal (tree-to-string (parse-string "[1 2 3]"))
             (list "array" "1" "2" "3")))
  (is (equal (tree-to-string (parse-string "[[1 2 3][4 5 6]]"))
             (list "array"
                   (list "array" "1" "2" "3")
                   (list "array" "4" "5" "6")))))

(test tuple
  (is (equal (tree-to-string (parse-string "{0 0 0}"))
             (list "tup" "0" "0" "0")))
  (is (equal (tree-to-string (parse-string "{1 3.14 \"test\"}"))
             (list "tup" "1" "3.14" "test"))))

(run! 'parser)
