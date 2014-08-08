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
