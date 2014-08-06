(in-package :cl-user)
(defpackage :corvus.macros
  (:use :cl :trivial-types :corvus.parser))
(in-package :corvus.macros)

;;; Data structures

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

;;; Macro definition

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
  (declare (type <sexp> tree)
           (type <macro-env> menv))
  (let ((name (val (first tree))))
    (setf (gethash name (macros menv))
          (parse-macro-definition (rest tree)))))

;;; Pattern matching

;; The environment

(defun env-append (pattern input env)
  (append env (list (list pattern input))))

(defun env-append-rest (pattern input env)
  (env-append pattern (list :rest
                            (if (atom input) (list input) input)) env))

;; Utilities

(defun var-p (string)
  (declare (type string string))
  (if (> (length string) 0)
      (char= (elt string 0) #\$)))

;;; Macroexpansion
