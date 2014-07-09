******
Syntax
******

Tokens
======

Token Types
-----------

The following list shows the regular expressions that correspond to each token
type:

* Integer: :code:`[+-]?(\d)+`.
* Float: :code:`[+-]?[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?`.

Identifiers are anything else.

Strings are not strictly tokens, as they are implemented as a reader macro.

Included Reader Macros
======================

Strings
-------

Strings are implemented as a reader macro. This encourages users to add new
string readers that serve different functions. For example, a user might create
a reader macro that takes a word, the reads the rest of the input until that
same word is found as a string.

Comments
--------

Two reader macros are defined to read comments. Single line comments begin with
the semicolon (:code:`;`) character, and end with a newline. Multi-line comments
are those of C: :code:`/* Comment text */`.

Multi-line comments can be nested.

Literals
--------

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
