(in-package :cl-user)
(defpackage :corvus.modules
  (:use :cl :anaphora :corvus.util :corvus.ast)
  (:export :modularize))
(in-package :corvus.modules)

;;; Data structures

(defclass <module> ()
  ((imports :initarg :imports :reader imports)
   (exports :initarg :exports :reader exports)))

(defclass <module-env> ()
  ((modules :initarg :modules :accessor modules)))

(defmethod get-module ((name string) (env <module-env>))
  (gethash name (modules env)))

(defmethod external-symbol-p ((name string) (mod <module>))
  (member name (exports mod) :test #'equal))

;;; Module definition

(defun find-subform (tree name)
  (declare (type <form> tree)
           (type string name))
  (loop for form in tree do
    (if (and (listp form)
             (typep (first form) '<atom>)
             (equal (val (first form)) name))
        (return-from find-subform (rest form)))))

(defun define-module (name body env)
  (loop for option in body do
    (let ((first (first option)))
      (unless (typep first '<identifier>)
        ;; TODO: Emit an error
        nil))))

;;; Modularizing: Appending module prefixes to an AST

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
  (if tree
      (if (typep tree '<atom>)
          (modularize-atom tree current-prefix)
          (cons (modularize (first tree) current-prefix)
                (modularize (rest tree) current-prefix)))
      ;;; The null form is returned as-is
      nil))

;;; Validate modules

(defun prefix (string)
  "Get the prefix from a string.

  \"module:symbol\" => \"module\""
  (if (has-prefix string)
      (subseq string 0 (position #\: string))))

(defun name (string)
  "Get the name from a prefixed string."
  (if (has-prefix string)
      (subseq string (1+ (position #\: string)))
      string))

(defun validate-ident (atom env)
  "Assumption: This is called after modularization, when all symbols have
prefixes."
  (let* ((text (val atom))
         (prefix (prefix text))
         (name (name text)))
    (aif (get-module prefix env)
             ;; The module exists. Is the symbol external to it?
             (if (external-symbol-p name it)
                 ;; The symbol is external in an existing module
                 atom
                 nil ;; TODO: Error: Symbol not external
                 )
             nil ;; TODO: Error: Module doesn't exist
             )))

(defun validate-tree (tree env)
  (if (typep tree '<atom>)
      (if (typep tree '<identifier>)
          (validate-ident tree env)
          tree)
      (cons (validate-tree (first tree) env)
            (validate-tree (rest tree) env))))
