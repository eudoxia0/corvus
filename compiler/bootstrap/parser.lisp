(in-package :cl-user)
(defpackage :corvus.parser
  (:use :cl)
  (:export :<token>
           :sexp))
(in-package :corvus.parser)

(defclass <atom> ()
  ((val :initarg :val :reader val :type string)))

(defmethod print-object ((atom <atom>) stream)
  (format stream "~A" (val atom)))

(deftype sexp
  `(or <atom>
       (and list
            (satisfies #'(lambda (list)
                           (every #'(lambda (elem) (typep elem 'sexp))
                                  list))))))

(defparameter +integer-scanner+
  (cl-ppcre:create-scanner "[+-]?(\\d)+."))

(defparameter +float-scanner+
  (cl-ppcre:create-scanner "[+-]?[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?"))

(defun integer-str-p (str)
  (if (cl-ppcre:scan +integer-scanner+ str) t))

(defun float-str-p (str)
  (if (cl-ppcre:scan +float-scanner+ str) t))

(defun atom (str line col)
  (make-instance '<atom> :val str))

(defclass <reader> ()
  ((stream :initarg stream :reader stream)
   (line :initform 0 :accessor line)
   (col :initform 0 :accessor col)))

(defun stdin-reader ()
  "Create a new Reader from stdin"
  (make-instance '<reader> :stream *standard-input*))

(defun file-reader (pathname)
  "Create a new Reader from a file"
  (make-instance '<reader> :stream (open pathname)))

(defparameter +reader+ nil)

(defun nextchar ()
  "Get the next character in the stream, advancing the cursor"
  (let ((char (read-char (stream +reader+) nil :eof)))
    (if (char= char #\Newline)
        (progn
          (setf (col +reader+) 0)
          (incf (line +reader+))
          char)
        (progn
          (incf (col +reader+))
          char))))

(defun make-atom (str)
  )

(defparameter +max-macro-len+ 6
  "This constant defines the maximum number of bytes a reader macro can have. By
   comparison, Common Lisp allows only two (Two-character macros are a single
   character prefixed by a 'dispatching macro character', typically #). This is
   set arbitrarily.")



(defun read-stream ()
  "For information on the reader algorithm, check the Reader chapter of the
   documentation."
  (let ((tok-text "")
        (char))
    (flet ((complete-token-p ()
             (> (length tok-text) 0))
           (make-atom ()
             (atom tok-text (line +reader+) (col +reader+))))
      (loop until (eq c :eof) do
        (setf c (next-char))
        ;; Match the character
        (cond ((whitespacep c)
               
               )
              (t
               ;; Any character that is not a macro character is a constituent
               ;; character of a token. At this point, a token begins to be
               ;; accumulated
               (setf tok-text (concatenate 'string tok-text (string c)))))
        ;; Handle terminating macro characters
        t)
      (if (complete-token-p)
          (make-atom)))))
    

(defun read-delim-sequence (delim)
  "A simple function to facilitate reading delimited sequences. It is used to
   read nested S-expressions, as well as array and tuple literals."
  nil)
