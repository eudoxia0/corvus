(in-package :cl-user)
(defpackage :corvus.ast
  (:use :cl :trivial-types)
  (:import-from :corvus.parser
                :ident-equal)
  (:export :annotate))
(in-package :corvus.ast)

;;; Built-in syntactic sugar

(defun desugar-bodies (expr)
  "Recur through the expression, desugaring the bodies of 'let's and 'lambda'."
  (if (atom expr)
      expr
      (let ((first (first expr))
            (args  (rest expr)))
        (cond
          ((or (ident-equal first "let") (ident-equal first "lambda"))
           ;; 'let' and 'lambda' have the same structure
           (let ((body (rest args)))
             (list first
                   (second expr)
                   (if (> (length body) 1)
                       (cons (make-instance 'corvus.parser:<identifier>
                                            :val "begin")
                             body)
                   body))))
          (t
           (cons (desugar-bodies (first expr))
                 (desugar-bodies (rest expr))))))))

(defun desugar-bindings (expr)
  "Recur through an expression, looking for 'let' expressions with multiple
bindings, turning them into recursive single-binding 'let' expressions."
  (if (atom expr)
      expr
      (let ((first (first expr))
            (args  (rest expr)))
        (cond
          ((ident-equal first "let")
           (let ((bindings (first args))
                 (body (rest args)))
             (if (> (length bindings) 1)
                 ;; Has more than one binding
                 (list first                   ;; let
                       (list (first bindings)) ;; ((var0 val0))
                       (append
                        (list first            ;; let
                              (rest bindings)) ;; ((var1 val1) ... (varn valn))
                        body))
                 expr)))
          (t
           (cons (desugar-bindings (first expr))
                 (desugar-bindings (rest expr))))))))

(defun desugar (expr)
  "To make type inference and compilation easier, certain structures undergo
  very simple transformations that have a great impact in the code's
  simplicity. For example:

  * A 'let' expression with multiple bindings is converted to a series of nested
    'let's.

  * The body of a 'let' or 'lambda' expression is converted into a 'begin'
    expression."
  (desugar-bodies (desugar-bindings expr)))

;;; AST definitions

(defclass <sexp> () ())

(defclass <atom> (<sexp>)
  ((val :initarg :val :reader val :type string)
   (line :initarg :line :reader line :initform 0)
   (col :initarg :col :reader col :initform 0)))

(defmethod print-object ((atom <atom>) stream)
  (format stream "~A" (val atom)))

(defclass <identifier> (<atom>) ())
(defclass <constant> (<atom>) ())
(defclass <integer> (<constant>) ())
(defclass <float> (<constant>) ())
(defclass <string> (<constant>) ())

(defclass <list> (<sexp>) ())

(defclass <begin> (<list>)
  ((steps :initarg :steps
          :reader steps
          :type (proper-list <ast>))))

(defclass <let> (<list>)
  ((var :initarg :var
        :reader var
        :type <identifier>)
   (val :initarg :val
        :reader val
        :type <ast>)
   (body :initarg :body
         :reader body
         :type <ast>)))
