;;;; This file is less a test of the utilities (compiler/bootstrap/util.lisp)
;;;; and more colection of utilities for tests.
(in-package :cl-user)
(defpackage :corvus-test.util
  (:use :cl :fiveam)
  (:export :is-type))
(in-package :corvus-test.util)

(defmacro is-type (expr type)
  `(is-true (typep ,expr ,type)))
