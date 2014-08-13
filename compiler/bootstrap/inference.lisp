;;;; The type inference engine is a reimplementation of Marc Feeley's
;;;; 'polytype', described as a 'polymorphic type inferencer for Scheme'.
;;;;
;;;; The original source code is available at:
;;;; http://www.cs.cmu.edu/afs/cs/Web/Groups/AI/lang/scheme/code/ext/types/polytype/polytype.scm

(in-package :corvus.types)

;;; Environment

(defun env-empty () (list))

(defun env-update (var val env)
  (cons (cons var val) env))

(defun env-append (env-a env-b)
  (append env-a env-b))

(defun env-val (var env)
  (cdr (assoc var env :test #'equal)))

;;; Type variables

(defparameter *var-count* 0
  "The ID of variables.")

(defclass <tvar> ()
  ((n :initarg :n :reader n :type integer :initform (incf *var-count*))))

(defun new-var ()
  (make-instance '<tvar>))

(defmethod print-object ((var <tvar>) stream)
  (format stream "?~A" (n var)))

(defmethod variable< ((x <tvar>) (y <tvar>))
  (< (n x) (n y)))

;;; Unification

(defun deref (var env)
  "Return the value of `var`, if it is bound in `env`. Otherwise, return `var`."
  (if (typep var '<tvar>)
      (aif (env-val var env)
           (deref it env)
           var)
      var))

(defun unify% (x y env)
  (cond ((equal x y)
         env)
        ((and (typep x '<tvar>)
              (or (not (typep y '<tvar>))
                  (variable< y x)))
         (env-update x y env))
        ((typep y '<tvar>)
         ;; If y is a variable, bind y to x
         (env-update y x env))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) env)))
        (t
         (error "Unification error"))))

(defun unify (x y env)
  (unify% (deref x env) (deref y env) env))

;;; Substitution

(defun subs (x env)
  "Recursively replace every variable with its value in the expression."
  (cond ((typep x '<tvar>)
         (let ((y (deref x env)))
           (if (typep y '<tvar>)
               y
               (subs y env))))
        ((listp x)
         (cons (subs (first x) env)
               (subs (rest x) env)))
        (t
         x)))

;;; Generics

;; The prefix, in the context of the following functions, refers to an
;; environment

(defun genericp (var prefix)
  "Recurs through the prefix, checking whether var is generic (ie was not
defined by a let)."
  (cond ((null prefix)
         t)
        ((and (equal (cadar prefix) var)
              (not (equal (caar prefix) 'let)))
         nil)
        (t
         (genericp var (rest prefix)))))

(defun new (x env success prefix)
  (cond ((and (typep x '<tvar>)
              (genericp x prefix))
         (aif (env-val x env)
              (funcall success it env)
              (let ((var (make-instance '<tvar>)))
                (funcall success var (env-update x var env)))))
        ((listp x)
         (new (first x) env (lambda (a env)
                              (new (rest x) env
                                   (lambda (b env)
                                     (funcall success (cons a b) env))
                                   prefix))
              prefix))))

(defun instance (x prefix)
  (new x (env-empty) (lambda (a env) (declare (ignore env)) a) prefix))

;;;

(defgeneric constant-type (constant)
  (:method ((int corvus.parser:<integer>))
    (make-instance '<i64>))
  (:method ((float corvus.parser:<float>))
    (make-instance '<double>))
  (:method ((str corvus.parser:<string>))
    (make-instance '<array>
                   :base-type (make-instance '<i8>))))

;;; The actual type inference machinery

(defgeneric algorithm-j (p f e)
  (:documentation "Robert Milner's Algorithm J."))

(defmethod algorithm-j (p (f corvus.parser:<constant>) e)
  (instance (constant-type f) (env-empty)))

(defmethod algorithm-j (p (f corvus.parser:<identifier>) e)
  (aif (env-val f p)
       (let ((kind (first it))
             (type (second it)))
         (if (eq kind 'let)
             (instance type p)
             type))
       ;; TODO: Return the type of a function with this name
       nil))

(defmethod algorithm-j (p (f list) e)
  "Since there are method for all subclasses of <atom>, this will work for lists.")

(defun infer (f)
  "Infer the type of 'f'."
  (declare (type <sexp> f))
  (let* ((e (env-empty))
         (term (algorithm-j (env-empty) f e)))
    (subs term e)))
