(in-package :cl-user)
(defpackage corvus-asd
  (:use :cl :asdf))
(in-package :corvus-asd)

(defsystem corvus
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-ppcre :esrap :trivial-types)
  :components ((:module "compiler/bootstrap"
                :serial t
                :components
                ((:file "parser")
                 (:file "util")
                 (:file "modules")
                 (:file "macros")
                 (:file "types"))))
  :description "Bootstrapping Corvus from Common Lisp")
