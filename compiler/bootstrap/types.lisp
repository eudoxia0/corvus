(in-package :cl-user)
(defpackage :corvus.types
  (:use :cl :trivial-types :anaphora)
  (:import-from :corvus.ast
                :<form>
                :<atom>
                :val
                :ident-equal)
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
           :<field>
           :<record>
           :<variant>
           :<datatype>
           :<type-env>
           :base-type
           :types
           :name
           :docstring
           :create-default-tenv
           :emit-type))
(in-package :corvus.types)

(defclass <type> () ()
  (:documentation "The superclass of all types."))

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

(defmethod width ((float <single>)) 32)
(defmethod width ((float <double>)) 64)
(defmethod width ((float <quad>)) 128)

;;;; Aggregate Types

(defclass <aggregate> (<type>) ())

;;; Arrays

(defclass <array> (<aggregate>)
  ((base-type :initarg :base-type :reader base-type :type <type>)))

;;; Tuples

(defclass <tuple> (<aggregate>)
  ((types :initarg :types :reader types :type (proper-list <type>))))

;;; Records

(defclass <field> ()
  ((name :initarg :name :reader name :type string)
   (base-type :initarg :base-type :reader base-type :type <type>)
   (docstring :initarg :docstring :reader docstring :type string)))

(defclass <record> (<aggregate>)
  ((fields :initarg :fields :reader fields :type (proper-list <type-field>))))

;;; Datatypes

(defclass <variant> ()
  ((name :initarg :name :reader name :type string)
   (base-type :initarg :base-type :reader base-type :type (or <type> null)))
  (:documentation "A variant of an ADT."))

(defclass <datatype> (<aggregate>)
  ((variants :initarg :variants
             :reader variants
             :type (proper-list <variant>)))
  (:documentation "An Algebraic Data Type."))

;;;; Type Environment

(defclass <type-env> ()
  ((types :initarg :types
          :accessor types
          :type hash-table
          :initform (make-hash-table :test #'equal)))
  (:documentation "A type environment associates names with type definitions."))

(defun create-default-tenv ()
  "Return a type environment with the basic types."
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
  "Emit the type specified by an atom: First validate that it is an identifier,
the look up the name in the type environment."
  (declare (type <atom> atom)
           (type <type-env> tenv))
  (let ((text (val atom)))
    (aif (gethash text (types tenv))
         it
         (error "No type named ~A in environment." text))))

(defun parse-type-field (field tenv)
  (let ((name (val (first field)))
        (base-type (emit-type (second field)
                              tenv))
        (docstring (aif (third field)
                        (val it)
                        "")))
    (make-instance '<field>
                   :name name
                   :base-type base-type
                   :docstring docstring)))

(defun parse-variant (field tenv)
  (let ((name (val (first field)))
        (base-type (aif (second field)
                        (emit-type it tenv)
                        nil)))
    (make-instance '<variant>
                   :name name
                   :base-type base-type)))

(defun emit-list (first args tenv)
  "Emit the type specified by a list. For example, `(tup i8 i8 i8)` is a list
that specifiers a triple of octets."
  (declare (type <form> first args)
           (type <type-env> tenv))
  (let ((fn (val first))) ;; TODO: Validate that it is in fact a token
    (cond
      ((equal fn "array")
       (make-instance '<array> :base-type (emit-type (first args) tenv)))
      ((equal fn "tup")
       (make-instance '<tuple> 
                      :types (mapcar #'(lambda (spec) (emit-type spec tenv))
                                     args)))
      ((equal fn "rec")
       ;; Loop for the fields in `args`, turning each into a `<type-field>`.
       (make-instance '<record>
                      :fields (mapcar #'(lambda (field)
                                          (parse-type-field field tenv))
                                      args)))
      ((equal fn "data")
       (make-instance '<datatype>
                      :variants (mapcar #'(lambda (field)
                                            (parse-variant field tenv))
                                        args)))
      (t
       (error "No type specifier '~A'." fn)))))

(defun emit-type (ast tenv)
  "Return the type specified by the type specifier `ast`."
  (declare (type <form> ast)
           (type <type-env> tenv))
  (if (null ast)
      (make-instance '<unit>)
      (if (typep ast '<atom>)
          (emit-atom ast tenv)
          (emit-list (first ast) (rest ast) tenv))))
