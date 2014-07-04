*********************
Phases of Compilation
*********************

Reader Macro Expansion
======================

Reader macro expansion happens at read time, while the source tree is being
parsed.

Module Expansion
================

Macroexpansion
==============

This is the second phase of compilation, before the AST is further
transformed. The compiler first looks through the AST, extracting macro
definitions, then traverses the AST again looking for calls to macros and performs the
macroexpansion process.

Type Definition Extraction
==========================

Since types are global, not scoped, they can be extracted all at once to build a
type environment before the actual type system is run.

Annotation
==========

The compiler turns the largely unstructured tree of S-expressions into a more
abstract tree, veryfying the syntax of certain special operations.

Variable Lifting
================

Variables are lifted out of their scopes.

For example, the following tree::

    (let (x 1
          y 2)
      (set x (+ x 1))
      (let (x 2)
        (print (+ x y))))

Would become something like::

    (let (x0 1
          y0 2)
      (set x0 (+ x0 1))
      (let (x1 2)
        (print (+ x1 y0))))

This allows us to hold in memory a single symbol table, rather than having to
recur or search through a nested one.

Closure Analysis
================

The compiler recurs through the blocks, finding the definitions of lambdas and
functions, annotating these functions with the variables (If any) that they
close over. Note that this is not lambda/function lifting.
