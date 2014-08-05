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

(run! 'parser)
