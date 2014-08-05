(in-package :cl-user)
(defpackage :corvus-test.parser
  (:use :cl :fiveam :corvus.parser))
(in-package :corvus-test.parser)

(def-suite parser
  :description "Testing the parser")
(in-suite parser)

(test identifier
  (is-true (typep (parse-string "test") '<identifier>))
  (is-true (typep (parse-string "a123") '<identifier>))
  (is-true (typep (parse-string "@test") '<identifier>))
  (is-true (typep (parse-string "3test3") '<identifier>))
  (is-true (typep (parse-string "test-test-test") '<identifier>)))

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

(run! 'parser)
