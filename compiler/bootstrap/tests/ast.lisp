(in-package :cl-user)
(defpackage :corvus-test.ast
  (:use :cl :fiveam :corvus.ast)
  (:import-from :corvus.parser
                :parse-string
                :equal-trees))
(in-package :corvus-test.ast)

(def-suite ast)
(in-suite ast)

(test bindings
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
            (parse-string "(let ((a 1)) (let ((b 2)) (let ((c 3)) body)))")))
  ;; Recursion
  (is-true (equal-trees
            (corvus.ast::desugar-bindings
             (parse-string "(let ((a 1) (b 2)) (let ((c 3) (d 4)) body))"))
            (parse-string "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) body))))")))
  (is-true (equal-trees
            (corvus.ast::desugar-bindings
             (parse-string "(let ((a 1)) (let ((b 2) (c 3)) body))"))
            (parse-string "(let ((a 1)) (let ((b 2)) (let ((c 3)) body)))"))))

(test bodies
  (is-true (equal-trees
            (corvus.ast::desugar-bodies
             (parse-string "(let binds body)"))
            (parse-string "(let binds body)")))
  (is-true (equal-trees
            (corvus.ast::desugar-bodies
             (parse-string "(let binds 1 2 3)"))
            (parse-string "(let binds (begin 1 2 3))")))
  (is-true (equal-trees
            (corvus.ast::desugar-bodies
             (parse-string "(lambda args body)"))
            (parse-string "(lambda args body)")))
  (is-true (equal-trees
            (corvus.ast::desugar-bodies
             (parse-string "(lambda args 1 2 3)"))
            (parse-string "(lambda args (begin 1 2 3))")))
  ;; Recursion
  (is-true (equal-trees
            (corvus.ast::desugar-bodies
             (parse-string "(let binds (let binds 1 2) (let binds a b))"))
            (parse-string "(let binds (begin (let binds (begin 1 2))
                                             (let binds (begin a b))))"))))

(run! 'ast)
