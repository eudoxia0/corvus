(in-package :cl-user)
(defpackage :corvus.types
  (:use :cl :trivial-types))
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
