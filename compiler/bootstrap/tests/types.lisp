(in-package :cl-user)
(defpackage :corvus-test.types
  (:use :cl :fiveam :corvus.types))
(in-package :corvus-test.types)

(def-suite types)
(in-suite types)

(test emit-atom
  (is-true (typep (emit-type nil (create-default-tenv))
                  '<unit>))
  (is-true (typep (emit-type (make-instance 'corvus.parser:<atom>
                                            :val "bool")
                             (create-default-tenv))
                  '<bool>)))

(run! 'types)
