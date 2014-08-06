(in-package :cl-user)
(defpackage :corvus-test.macros
  (:use :cl :fiveam :corvus.macros))
(in-package :corvus-test.macros)

(def-suite macros)
(in-suite macros)

(run! 'macros)
