******
Syntax
******

Reader Algorithm
================

Corvus' reader algorithm is loosely based on Common Lisp's, as `described
<http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm>`_ in the Common
Lisp Specification.

Included Reader Macros
======================

Comments
--------

Two reader macros are defined to read comments. Single line comments begin with
the semicolon (';') character, and end with a newline. Multi-line comments are
those of C: :code:`/* Comment text */`.

Multi-line comments can be nested.

Literals
^^^^^^^^

Reader macros are defined to create tuple and array literals. These are,
respectively:

* Curly braces: Tuple elements are delimited by a matching pair of curly braces,
  and separated by spaces.

  Examples:

  * :code:`{1 2 3}`: A triple of integers.
  * :code:`{1 3.14}`: A pair of an integer and a float.
* Brackets: Same as the curly braces macro, only using braces.
  Examples:
  * :code:`[1 2 3]`: An array of three integers.

Syntax Conventions
==================

Indentation
-----------

