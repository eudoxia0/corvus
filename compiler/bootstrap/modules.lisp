(in-package :cl-user)
(defpackage :corvus.modules
  (:use :cl :corvus.util :corvus.parser)
  (:export :modularize))
(in-package :corvus.modules)

;;; Data structures

(defclass <module> ()
  ((imports :initarg :imports :reader imports)
   (exports :initarg :exports :reader exports)))

(defclass <module-env> ()
  ((modules :initarg :modules :accessor modules)))

;;; Module definition

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

;;; Appending module prefixes to an AST

(defun has-prefix (string)
  "Return `T` if the string has module prefix."
  (declare (type string string))
  (if (position #\: string) t))

(defun add-prefix (atom prefix)
  (make-instance '<atom>
                 :val (concatenate 'string prefix ":" (val atom))))

(defun modularize-atom (atom current-prefix)
  (declare (type <atom> atom)
           (type string current-prefix))
  (if (and (typep atom '<identifier>)
           (not (has-prefix (val atom))))
      ;; No prefix, add that of the current module
      (add-prefix atom current-prefix)
      ;; If it has a prefix or isn't an identifier, leave it as-is
      atom))

(defun modularize (tree current-prefix)
  (if (typep tree '<atom>)
      (modularize-atom tree current-prefix)
      (cons (modularize (first tree) current-prefix)
            (modularize (rest tree) current-prefix))))
