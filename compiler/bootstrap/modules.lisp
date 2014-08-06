(in-package :cl-user)
(defpackage :corvus.modules
  (:use :cl :corvus.util))
(in-package :corvus.modules)

(defclass <module> ()
  ((imports :initarg :imports :reader imports)
   (exports :initarg :exports :reader exports)))

(defclass <module-env> ()
  ((modules :initarg :modules :accessor modules)))

(defun find-subform (tree name)
  (declare (type <sexp> tree)
           (type string name))
  (loop for form in tree do
    (if (and (listp form)
             (typep (first form) '<atom>)
             (equal (val (first form)) name))
        (return-from find-subform (rest form)))))

(defun process-module-definitions (tree env)
  nil)
