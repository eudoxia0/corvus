(in-package :cl-user)
(defpackage :corvus.types
  (:use :cl))
(in-package :corvus.types)

(defclass <type> () ())

;;;; Scalar types

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
