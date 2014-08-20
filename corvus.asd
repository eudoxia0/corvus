(in-package :cl-user)
(defpackage corvus-asd
  (:use :cl :asdf))
(in-package :corvus-asd)

(defsystem corvus
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-ppcre :esrap :trivial-types :anaphora)
  :components ((:module "compiler/bootstrap"
                :serial t
                :components
                ((:file "util")
                 (:file "ast")
                 (:file "parser")
                 (:file "modules")
                 (:file "macros")
                 (:file "types")
                 (:file "inference"))))
  :description "Bootstrapping Corvus from Common Lisp")
