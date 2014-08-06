(in-package :cl-user)
(defpackage :corvus-test.parser
  (:use :cl :fiveam :corvus.parser))
(in-package :corvus-test.parser)

(def-suite parser)
(in-suite parser)

(test integer
  (is-true (typep (parse-string "1") '<integer>))
  (is-true (typep (parse-string "1234") '<integer>))
  (is-true (typep (parse-string "+213213") '<integer>))
  (is-true (typep (parse-string "-23232") '<integer>)))

(test float
  (is-true (typep (parse-string "23.0") '<float>))
  (is-true (typep (parse-string "3.14") '<float>))
  (is-true (typep (parse-string "+3.14") '<float>))
  (is-true (typep (parse-string "-3.14") '<float>))
  (is-true (typep (parse-string "45e10") '<float>))
  (is-true (typep (parse-string "-10e2") '<float>)))

(test string
  (is-true (typep (parse-string "\"\"") '<string>))
  (is-true (typep (parse-string "\"test\"") '<string>))
  (is (equal (val (parse-string "\"test\""))
             "test")))

(test identifier
  (is-true (typep (parse-string "test") '<identifier>))
  (is-true (typep (parse-string "a123") '<identifier>))
  (is-true (typep (parse-string "@test") '<identifier>))
  (is-true (typep (parse-string "3test3") '<identifier>))
  (is-true (typep (parse-string "test-test-test") '<identifier>)))

(defun tree-to-string (tree)
  (if tree
      (if (atom tree)
          (val tree)
          (cons (tree-to-string (first tree))
                (tree-to-string (rest tree))))))

(test list
  (is-true (null (parse-string "()")))
  (is-true (typep (parse-string "(1 2 3)") 'list))
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
