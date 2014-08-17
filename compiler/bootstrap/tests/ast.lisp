(in-package :cl-user)
(defpackage :corvus-test.ast
  (:use :cl :fiveam :corvus.ast)
  (:import-from :corvus.parser
                :parse-string
                :equal-trees))
(in-package :corvus-test.ast)

(def-suite ast)
(in-suite ast)

(test let
  (is-true (equal-trees
            (corvus.ast::desugar-bindings
             (parse-string "(let ((a 1)) body)"))
            (parse-string "(let ((a 1)) body)")))
  (is-true (equal-trees
            (corvus.ast::desugar-bindings
             (parse-string "(let ((a 1) (b 2)) body)"))
            (parse-string "(let ((a 1)) (let ((b 2)) body))")))
  (is-true (equal-trees
            (corvus.ast::desugar-bindings
             (parse-string "(let ((a 1) (b 2) (c 3)) body)"))
            (parse-string "(let ((a 1)) (let ((b 2)) (let ((c 3)) body)))"))))

(run! 'ast)
