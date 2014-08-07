(in-package :cl-user)
(defpackage :corvus-test.modules
  (:use :cl :fiveam :corvus.modules)
  (:import-from :corvus.parser
                :val
                :parse-string
                :tree-to-string))
(in-package :corvus-test.modules)

(def-suite modules)
(in-suite modules)

(test prefix
  (is-true (corvus.modules::has-prefix "module:symbol"))
  (is-false (corvus.modules::has-prefix "symbol"))
  (is (equal (val (corvus.modules::modularize-atom (parse-string "test")
                                                   "test-module"))
             "test-module:test"))
  (is (equal (val (modularize (parse-string "test")
                              "test-module"))
             "test-module:test")))

(test modularize
  (is (equal (tree-to-string (modularize (parse-string "(fn 1 3.14 \"test\")")
                                         "test-module"))
             (list "test-module:fn" "1" "3.14" "test")))
  (is (equal (tree-to-string (modularize (parse-string "((a) (b))")
                                         "test-module"))
             (list (list "test-module:a") (list "test-module:b"))))
  (is (equal (tree-to-string (modularize (parse-string "(a:a b c:c)")
                                         "test-module"))
             (list "a:a" "test-module:b" "c:c"))))

(run! 'modules)
