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

(defun process-cases (tree)
  (declare (type <sexp> tree))
  (loop for (pattern template) on tree by #'cddr collecting
    (make-instance '<macro-case>
                   :pattern pattern
                   :template template)))

(defun parse-macro-definition (tree)
  (declare (type <sexp> tree))
  (make-instance '<macro>
                 :cases (process-cases tree)))

(defun define-macro (tree menv)
  (let ((name (val (first tree))))
    (setf (gethash name (macros menv))
          (parse-macro-definition (rest tree)))))
