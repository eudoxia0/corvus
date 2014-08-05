(in-package :cl-user)
(defpackage :corvus.modules
  (:use :cl :corvus.util))
(in-package :corvus.modules)

(defclass <module> ()
  ((imports :initarg :imports :reader imports)
   (exports :initarg :exports :reader exports)))

(defclass <module-env> ()
  ((modules :initarg :modules :accessor modules)))

(defun process-module-definitions (tree env)
  nil)
