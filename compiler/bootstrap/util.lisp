(in-package :cl-user)
(defpackage :corvus.util
  (:use :cl)
  (:export :slurp-file))
(in-package :corvus.util)

(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character
                                                :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))
