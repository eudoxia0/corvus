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

The following will never be implemented:

* Garbage Collection.
* Exceptions.

History
=======

Notational Conventions
======================

Specially in syntax definitions, the following rules are used:

* Text in angular brackets represents a variable, eg. `<var>`, `<args>`,
  `<val>`.
* The following quantifiers, when found after a variable, represent that the
  variable:
  * `+`: Consists of at least one expression.
  * `*`: Consists of zero or more expressions.
  * `?`: Is optional.
* Brackets may be used to group variables that are not other wise delimited. For
  example, the syntax description `[<var> <val>]+` would describe expressions
  such as `a 1 b 2 c 3`.
