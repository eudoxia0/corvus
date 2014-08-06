(in-package :cl-user)
(defpackage :corvus.types
  (:use :cl :trivial-types :anaphora)
  (:import-from :corvus.parser
                :<sexp>
                :<atom>
                :val)
  (:export :<type>
           :<unit>
           :<bool>
           :<true-literal>
           :<false-literal>
           :<integer>
           :<i8>
           :<i16>
           :<i32>
           :<i64>
           :<i128>
           :<float>
           :<single>
           :<double>
           :<quad>
           :width
           :<aggregate>
           :<array>
           :<tuple>
           :<type-field>
           :<record>
           :<variant>
           :<datatype>
           :<type-env>
           :create-default-tenv
           :emit-type))
(in-package :corvus.types)

(defclass <type> () ())

;;;; Scalar types

(defclass <unit> (<type>) ())
(defclass <bool> (<type>) ())

(defclass <true-literal> (<bool>) ())
(defclass <false-literal> (<bool>) ())

;;; Integer types

(defclass <integer> (<type>) ())

(defclass <i8> (<integer>) ())
(defclass <i16> (<integer>) ())
(defclass <i32> (<integer>) ())
(defclass <i64> (<integer>) ())
(defclass <i128> (<integer>) ())

(defgeneric width (type)
  (:method ((int <i8>)) 8)
  (:method ((int <i16>)) 16)
  (:method ((int <i32>)) 32)
  (:method ((int <i64>)) 64)
  (:method ((int <i128>)) 128))

;;; Floating-point types

(defclass <float> (<type>) ())

(defclass <single> (<float>) ())
(defclass <double> (<float>) ())
(defclass <quad> (<float>) ())

(defgeneric width (type)
  (:method ((float <single>)) 32)
  (:method ((float <double>)) 64)
  (:method ((float <quad>)) 128))

;;;; Aggregate Types

(defclass <aggregate> (<type>) ())

;;; Arrays

(defclass <array> (<aggregate>)
  ((base-type :initarg :base-type :reader base-type :type <type>)))

;;; Tuples

(defclass <tuple> (<aggregate>)
  ((types :initarg :types :reader types :type (proper-list <type>))))

;;; Records

(defclass <type-field> ()
  ((name :initarg :name :reader name :type string)
   (base-type :initarg :base-type :reader base-type :type <type>)
   (docstring :initarg :docstring :reader docstring :type string)))

(defclass <record> (<aggregate>)
  ((fields :initarg :fields :reader fields :type (proper-list <type-field>))))

;;; Datatypes

(defclass <variant> ()
  ((name :initarg :name :reader name :type string)
   (base-type :initarg :base-type :reader base-type :type <type>)))

(defclass <datatype> (<aggregate>)
  ((variants :initarg :variants
             :reader variants
             :type (proper-list <variant>))))

;;;; Type Environment

(defclass <type-env> ()
  ((types :initarg :types
          :accessor types
          :type hash-table
          :initform (make-hash-table :test #'equal))))

(defun create-default-tenv ()
  (let ((tenv (make-instance '<type-env>)))
    (setf (gethash "bool" (types tenv))
          (make-instance '<bool>)
          (gethash "i8" (types tenv))
          (make-instance '<i8>)
          (gethash "i16" (types tenv))
          (make-instance '<i16>)
          (gethash "i32" (types tenv))
          (make-instance '<i32>)
          (gethash "i64" (types tenv))
          (make-instance '<i64>)
          (gethash "i128" (types tenv))
          (make-instance '<i128>)
          (gethash "single" (types tenv))
          (make-instance '<single>)
          (gethash "double" (types tenv))
          (make-instance '<double>)
          (gethash "quad" (types tenv))
          (make-instance '<quad>))
    tenv))

;;;; Type Specifiers

(defun emit-atom (atom tenv)
  (declare (type <atom> atom)
           (type <type-env> tenv))
  (let ((text (val atom)))
    (aif (gethash text (types tenv))
         it
         (error "No type named ~A in environment." text))))

(defun emit-list (first args tenv)
  (declare (type <sexp> first args)
           (type <type-env> tenv))
  (make-instance '<unit>))

(defun emit-type (ast tenv)
  (declare (type <sexp> ast)
           (type <type-env> tenv))
  (if (null ast)
      (make-instance '<unit>)
      (if (typep ast '<atom>)
          (emit-atom ast tenv)
          (emit-list (first ast) (rest ast) tenv))))
