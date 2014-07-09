************
Introduction
************

Corvus is a statically-typed, low-level dialect of Lisp.

Features
========

* Static typing with partial inference.
* Manual memory management with multiple safety guarantees.
* Common Lisp Conditions and Restarts for Error Handling.
* Macros and Common Lisp-style reader macros.
* Runtime optional.

Anti-Features
=============

The following will never be required:

* Garbage Collection.
* Exceptions.

History
=======

Notational Conventions
======================

Specially in syntax definitions, the following rules are used:

* Text in angular brackets represents a variable, eg. :code:`<var>`,
  :code:`<args>`, :code:`<val>`.
* The following quantifiers, when found after a variable, represent that the
  variable:
  * :code:`+`: Consists of at least one expression.
  * :code:`*`: Consists of zero or more expressions.
  * :code:`?`: Is optional.
* Brackets may be used to group variables that are not other wise delimited. For
  example, the syntax description :code:`[<var> <val>]+` would describe
  expressions such as :code:`a 1 b 2 c 3`.
