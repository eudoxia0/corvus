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
