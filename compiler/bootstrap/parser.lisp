(in-package :cl-user)
(defpackage :corvus.parser
  (:use :cl :esrap)
  (:import-from :corvus.ast
                :<identifier>
                :<integer>
                :<float>)
  (:export :parse-string
           :parse-file))
(in-package :corvus.parser)

(defparameter +integer-scanner+
  (cl-ppcre:create-scanner "^([+-]?(\\d)+)$"))

(defparameter +float-scanner+
  (cl-ppcre:create-scanner "^([+-]?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?)$"))

(defun integer-str-p (str)
  (if (cl-ppcre:scan +integer-scanner+ str) t))

(defun float-str-p (str)
  (if (cl-ppcre:scan +float-scanner+ str) t))

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule valid-char (not (or whitespace #\( #\) #\[ #\] #\{ #\})))

(defrule atom (+ valid-char)
  (:lambda (list)
    (let ((text (text list)))
      (make-instance
       (cond
         ((integer-str-p text)
          '<integer>)
         ((float-str-p text)
          '<float>)
         (t
          '<identifier>))
       :val text))))

(defrule string-char (not #\"))

(defrule string (and #\" (* string-char) #\")
  (:destructure (open text close)
    (declare (ignore open close))
    (make-instance '<string> :val (text text))))

(defrule list (and #\( (* sexp) (? whitespace) #\))
 (:destructure (p1 body w p2)
   (declare (ignore p1 w p2))
   body))

(defrule array-literal (and #\[ (* sexp) (? whitespace) #\])
  (:destructure (b1 body w b2)
    (declare (ignore b1 w b2))
    (cons (make-instance '<identifier> :val "array")
          body)))

(defrule tuple-literal (and #\{ (* sexp) (? whitespace) #\})
  (:destructure (cb1 body w cb2)
    (declare (ignore cb1 w cb2))
    (cons (make-instance '<identifier> :val "tup")
          body)))

(defrule sexp (and (? whitespace)
                   (or list array-literal tuple-literal string atom)
                   (? whitespace))
  (:destructure (left-ws text right-ws)
    (declare (ignore left-ws right-ws))
    (first (list text))))

;;; Interface

(defun parse-string (string)
  (parse 'sexp string))

(defun parse-file (pathname)
  (parse-string (corvus.util:slurp-file pathname)))
