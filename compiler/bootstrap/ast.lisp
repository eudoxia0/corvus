(in-package :cl-user)
(defpackage :corvus.ast
  (:use :cl :trivial-types)
  (:export :annotate))
(in-package :corvus.ast)

;;; Built-in syntactic sugar

(defun desugar-body (body)
  "Take a list of expressions. If it has more than one expr, turn it into a
'begin' statement."
  (if (> (length body) 1)
      (cons (make-instance 'corvus.parser:<identifier>
                           :val "begin")
            body)))

(defun desugar-bodies (expr)
  "Recur through the expression, desugaring the bodies of 'let's and 'lambda'."
  (if (atom expr)
      expr
      (let ((first (first expr))
            (args  (rest expr)))
        (cond
          ((or (ident-equal first "let") (ident-equal first "lambda"))
           ;; 'let' and 'lambda' have the same structure
           (list first (second expr) (desugar-body (rest args))))
          (t
           (cons (desugar-bodies (first expr))
                 (desugar-bodies (rest expr))))))))

(defun desugar (expr)
  "To make type inference and compilation easier, certain structures undergo
  very simple transformations that have a great impact in the code's
  simplicity. For example:

  * A 'let' expression with multiple bindings is converted to a series of nested
    'let's.

  * The body of a 'let' or 'lambda' expression is converted into a 'begin'
    expression."
  (desugar-bodies expr))
