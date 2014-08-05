(in-package :cl-user)
(defpackage :corvus.util
  (:use :cl :corvus.parser)
  (:export :find-subform))
(in-package :corvus.util)

(defun find-subform (tree name)
  (declare (type <sexp> tree)
           (type string name))
  (loop for form in tree do
    (if (and (listp form)
             (typep (first form) '<atom>)
             (equal (val (first form)) name))
        (return-from find-subform (rest form)))))
             
