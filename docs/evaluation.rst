**********
Evaluation
**********

Input Modes
===========

There are two required input modes for the compiler: The REPL and the
command-line interface. The operation of the compiler is similar for both.

This specification does not *require* implementations to present any information
(Version number, license information, etc.) to the user in any input mode.

REPL
----

The REPL (Read-Eval-Print-Loop) provides an interactive interface to the
language. The REPL works as follows:

* A prompt is presented to the user. See the configuration section.
* The reader is called, which waits until the user produces an entire form of
  input.
* The input is compiled and executed, and the value of the output is shown to
  the user.
* A newline is printed to the screen, and the loop repeats.

Command-Line Interface
----------------------

* The compiler is given, through command-line arguments, a list (Not a set) of
  file paths to compile.
* Each file path is processed in the order in which they were given to the
  compiler [#f1]_.
* For each file, the compiler:

  * Checks that the file pointed to by the path exists, and if not, halts the
    entire compilation process.
  * Compiles the file pointed to by the path. By default, an object code file is
    produced for each file, unless the compiler is requested to abort at a
    particular phase of compilation.

.. rubric:: Footnotes

.. [#f1] The files are not checked for existence before being processed. This is
         because later they may no longer be present once the first files have
         been compiled.


Scope
=====

The scoping rules are as follows:

* **Type definitions** may occur anywhere, but are global, and cannot be
  shadowed. Moreover, type definitions are extracted from the form being
  compiled before any other code is actually compiled.
* **Variables** are lexically-scoped, and variables in inner scopes shadow
  variables in outer scopes with the same name.
* **Functions** are also lexically scoped: If two functions with the same name
  and the same argument list are declared, a call in the innermost scope will
  call the innermost function.
