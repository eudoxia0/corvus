(in-package :cl-user)
(defpackage :corvus-test.modules
  (:use :cl :fiveam :corvus.modules)
  (:import-from :corvus.parser
                :val
                :parse-string))
(in-package :corvus-test.modules)

(def-suite modules)
(in-suite modules)

(test prefix
  (is-true (corvus.modules::has-prefix "module:symbol"))
  (is-false (corvus.modules::has-prefix "symbol"))
  (is (equal (val (corvus.modules::modularize-atom (parse-string "test")
                                                   "test-module"))
             "test-module:test")))

(run! 'modules)
