(in-package :cl-user)
(defpackage :corvus-test.types
  (:use :cl :fiveam :corvus.types)
  (:import-from :corvus.parser
                :parse-string))
(in-package :corvus-test.types)

(def-suite types)
(in-suite types)

(defun emit-parsed (string)
  (emit-type (parse-string string) (create-default-tenv)))

(test emit-atom
  (is-true (typep (emit-parsed "()") '<unit>))
  (is-true (typep (emit-parsed "bool") '<bool>)))

(test emit-array
  (is-true (typep (emit-parsed "(array i8)") '<array>))
  (is-true (typep (emit-parsed "[i8]") '<array>)))

(test emit-tuple
  (is-true (typep (emit-parsed "(tup i8 i8 i8)") '<tuple>))
  (is-true (typep (emit-parsed "{i8 i16 i32}") '<tuple>)))

(test emit-record
  (is-true (typep (corvus.types::parse-type-field (parse-string "(name bool)")
                                                  (create-default-tenv))
                  '<field>))
  (is (equal (name (corvus.types::parse-type-field (parse-string "(name bool)")
                                                  (create-default-tenv)))
             "name"))
  (is-true (typep (base-type (corvus.types::parse-type-field
                              (parse-string "(name bool)")
                              (create-default-tenv)))
                  '<bool>)))

(test emit-datatype
  (is-true (typep (corvus.types::parse-variant (parse-string "(Nil)")
                                               (create-default-tenv))
                  '<variant>))
  (is-true (typep (corvus.types::parse-variant (parse-string "(Byte i8)")
                                               (create-default-tenv))
                  '<variant>)))

(run! 'types)
