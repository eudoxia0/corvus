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
                ((:file "parser")
                 (:file "types"))))
  :description "Bootstrapping Corvus from Common Lisp")
