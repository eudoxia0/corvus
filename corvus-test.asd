(in-package :cl-user)
(defpackage corvus-asd
  (:use :cl :asdf))
(in-package :corvus-asd)

(defsystem corvus-test
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:fiveam :corvus)
  :components ((:module "compiler/bootstrap/tests"
                :serial t
                :components
                ((:file "util")
                 (:file "parser") ;; The parser is tested before the AST,
                                  ;; because the AST tests use `parse-string` to
                                  ;; simplify things.
                 (:file "ast")
                 (:file "modules")
                 (:file "macros")
                 (:file "types"))))
  :description "Bootstrapping Corvus from Common Lisp")
