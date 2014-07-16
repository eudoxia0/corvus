(in-package :cl-user)
(defpackage corvus-asd
  (:use :cl :asdf))
(in-package :corvus-asd)

(defsystem corvus
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-ppcre)
  :components ((:module "compiler"
                :components
                ((:module "bootstrap"
                  :components
                  ((:file "parser")
                   (:file "macros")
                   (:file "modules")
                   (:file "types"))))))
  :description "Bootstrapping Corvus from Common Lisp")
