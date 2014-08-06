(in-package :cl-user)
(defpackage :corvus.parser
  (:use :cl :esrap)
  (:export :<token>
           :<atom>
           :<integer>
           :<float>
           :<string>
           :<identifier>
           :val
           :line
           :col
           :<sexp>
           :parse-string
           :parse-file))
(in-package :corvus.parser)

(defclass <atom> ()
  ((val :initarg :val :reader val :type string)
   (line :initarg :line :reader line :initform 0)
   (col :initarg :col :reader col :initform 0)))

(defmethod print-object ((atom <atom>) stream)
  (format stream "~A" (val atom)))

(defclass <integer> (<atom>) ())
(defclass <float> (<atom>) ())
(defclass <string> (<atom>) ())
(defclass <identifier> (<atom>) ())

(deftype <sexp> ()
  '(or <atom> list))

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

(defrule list (and #\( sexp (* sexp) (? whitespace) #\))
 (:destructure (p1 car cdr w p2)
   (declare (ignore p1 w p2))
   (cons car cdr)))

(defrule array-literal (and #\[ sexp (* sexp) (? whitespace) #\])
  (:destructure (b1 car cdr w b2)
    (declare (ignore b1 w b2))
    (cons (make-instance '<identifier> :val "array")
          (cons car cdr))))

(defrule tuple-literal (and #\{ sexp (* sexp) (? whitespace) #\})
  (:destructure (cb1 car cdr w cb2)
    (declare (ignore cb1 w cb2))
    (cons (make-instance '<identifier> :val "tup")
          (cons car cdr))))

(defrule sexp (and (? whitespace)
                   (or list array-literal tuple-literal string atom)
                   (? whitespace))
  (:destructure (left-ws text right-ws)
    (declare (ignore left-ws right-ws))
    (first (list text))))

(defun parse-string (string)
  (parse 'sexp string))

(defun parse-file (pathname)
  (parse-string (corvus.util:slurp-file pathname)))
