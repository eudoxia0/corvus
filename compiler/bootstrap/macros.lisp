(in-package :cl-user)
(defpackage :corvus.macros
  (:use :cl :trivial-types :corvus.parser))
(in-package :corvus.macros)

(defclass <macro-case> ()
  ((pattern :initarg :pattern
            :reader pattern
            :type <sexp>)
   (template :initarg :template
             :reader template
             :type <sexp>)))

(defclass <macro> ()
  ((cases :initarg :cases
          :reader cases
          :type (proper-list <macro-case>))))

(defclass <macro-env> ()
  ((macros :initarg :macros
           :reader macros
           :type hash-table
           :initform (make-hash-table :test #'equal))))
