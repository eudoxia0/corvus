(in-package :cl-user)
(defpackage :corvus.parser
  (:use :cl)
  (:export :<token>
           :sexp
           :parse-string
           :parse-io
           :parse-file))
(in-package :corvus.parser)

(defclass <atom> ()
  ((val :initarg :val :reader val :type string)
   (line :initarg :line :reader line :initform 0)
   (col :initarg :col :reader col :initform 0)))

(defmethod print-object ((atom <atom>) stream)
  (format stream "~A" (val atom)))

(deftype sexp ()
  '(or <atom> list))

(defparameter +integer-scanner+
  (cl-ppcre:create-scanner "[+-]?(\\d)+."))

(defparameter +float-scanner+
  (cl-ppcre:create-scanner "[+-]?[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?"))

(defun integer-str-p (str)
  (if (cl-ppcre:scan +integer-scanner+ str) t))

(defun float-str-p (str)
  (if (cl-ppcre:scan +float-scanner+ str) t))

(defclass <reader> ()
  ((buf :initarg :buf :reader buf)
   (line :initform 0 :accessor line)
   (col :initform 0 :accessor col)))

(defun stdin-reader ()
  "Create a new Reader from stdin"
  (make-instance '<reader> :buf *standard-input*))

(defun file-reader (pathname)
  "Create a new Reader from a file"
  (make-instance '<reader> :buf (open pathname)))

(defun string-reader (str)
  "Create a new <reader> from a string"
  (with-input-from-string (buf str)
    (make-instance '<reader> :buf buf)))

(defparameter +reader+ nil)

(defun next-char (reader)
  "Get the next character in the buf, advancing the cursor"
  (let ((char (read-char (buf reader) nil :eof)))
    (if (char= char #\Newline)
        (progn
          (setf (col reader) 0)
          (incf (line reader))
          char)
        (progn
          (incf (col reader))
          char))))

(defparameter +max-macro-len+ 6
  "This constant defines the maximum number of bytes a reader macro can have. By
   comparison, Common Lisp allows only two (Two-character macros are a single
   character prefixed by a 'dispatching macro character', typically #). This is
   set arbitrarily.")

(defun read-stream (reader)
  "For information on the reader algorithm, check the Reader chapter of the
   documentation."
  (let ((tok-text "")
        (char))
    (flet ((complete-token-p ()
             (> (length tok-text) 0))
           (make-atom ()
             (make-instance '<atom>
                            :val tok-text
                            :line (line reader)
                            :col (col reader))))
      (loop do
        (setf char (next-char reader))
        ;; Match the character
        (cond ((eql char :eof)
               (if (complete-token-p)
                   (make-atom)))
              ((or (cl-ppcre::whitespacep char)
                   (char= (peek-char nil (buf reader) :eof) #\)))
               ;; If c is a terminating character, discard it and re-enter the
               ;; loop, unless we are reading a token, in which case the
               ;; terminator ends it.
               (if (complete-token-p)
                   (return-from read-stream (make-atom))
                   (continue)))
               ;; If c is a dispatching or non-dispatching macro character, its
               ;; associated function is called. Until I implement reader macros
               ;; satisfactorily, we'll cheat and directly implement the
               ;; 'macros' for parentheses and quotes.
              ((char= char #\()
               (return-from read-stream (read-delim-sequence reader #\))))
              (t
               ;; Any character that is not a macro character is a constituent
               ;; character of a token. At this point, a token begins to be
               ;; accumulated
               (setf tok-text (concatenate 'string tok-text (string char)))))))))

(defun read-delim-sequence (reader delim)
  "A simple function to facilitate reading delimited sequences. It is used to
   read nested S-expressions, as well as array and tuple literals."
  (let ((current (read-stream reader))
        (elems (list)))
    (flet ((terminatingp ()
             (char= delim (elt (val current)
                               (1- (length (val current)))))))
      (loop do
	(if (and (atom current) (terminatingp))
            (progn
              (setf elems (append elems (list current)))
              (return-from read-delim-sequence elems)))
        (print current)
        (setf elems (append elems (list current)))
        (setf current (read-stream reader))))
    elems))

(defun parse-string (str)
  (read-stream (string-reader str)))

(defun parse-io ()
  (read-stream (stdin-reader)))

(defun parse-file (pathname)
  (read-stream (file-reader pathname)))
