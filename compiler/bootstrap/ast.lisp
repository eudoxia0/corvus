;;;; Corvus has two ASTs: A "naive" ast, where each form (Type <form>) is either
;;;; a list or an <atom>, and a "full ast" (Type <ast>), where lists are further
;;;; refined into more specific types, like 'let' and 'lambda' forms. The output
;;;; of the parser is this naive AST, the annotate function performs some
;;;; transformations to simplify the syntax (See desugar) and then processes the
;;;; lists into the more specific classes.
(in-package :cl-user)
(defpackage :corvus.ast
  (:use :cl :trivial-types)
  (:export :<ast>
           :<atom>
           :val
           :line
           :col
           :<identifier>
           :<constant>
           :<integer>
           :<float>
           :<string>
           :<list>
           :<begin>
           :steps
           :<let>
           :var
           :val
           :body
           :<form>))
(in-package :corvus.ast)

;;;; Abstract syntax tree class definitions

;;; The full AST

(defclass <ast> () ())

(defclass <atom> (<ast>)
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

(defclass <list> (<ast>) ())

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

;;; The naive AST (This type exists for the purposes of annotation, and
;;; otherwise doesn't do much).

(deftype <form> ()
  '(or <atom> list))

;;;; Built-in syntactic sugar

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
             (append (list first
                           (first args))
                     (desugar-bodies
                      (if (> (length body) 1)
                          (list (cons (make-instance '<identifier>
                                                     :val "begin")
                                      body))
                          body)))))
          (t
           (cons (desugar-bodies (first expr))
                 (desugar-bodies (rest expr))))))))

(defun desugar-bindings (expr)
  "Recur through an expression, looking for 'let' expressions with multiple
bindings, turning them into recursive single-binding 'let' expressions. MUST be
run after desugar-bodies."
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
                       (desugar-bindings
                        (append
                         (list first             ;; let
                               (rest bindings)) ;; ((var1 val1) ... (varn valn))
                         (desugar-bindings body))))
                 (list (first expr) (second expr) (desugar-bindings (third expr))))))
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
  (desugar-bindings (desugar-bodies expr)))

;;; Utilities

(defun tree-to-string (tree)
  "Convert an S-expression into a tree of strings."
  (if tree
      (if (atom tree)
          (val tree)
          (cons (tree-to-string (first tree))
                (tree-to-string (rest tree))))))

(defun ident-equal (token str)
  (and (typep token '<identifier>)
       (equal (val token) str)))

(defun equal-trees (tree-a tree-b)
  (let ((tree-a* (tree-to-string tree-a))
        (tree-b* (tree-to-string tree-b)))
    (equal tree-a* tree-b*)))
