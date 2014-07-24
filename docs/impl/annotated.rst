*********************
Annotated Source Code
*********************

:code:`list.rs`
===============

The :code:`list.rs` module contains the definition of a data structure that is
essentially a "generalized S-expression", defined as:

.. literalinclude:: ../../compiler/list.rs
   :language: rust
   :lines: 1-5
   :linenos:

The implementation was moved out of :code:`ast.rs` because other parts of the
compiler use the same structure for holding other types of data (Instead of,
say, vectors) because of its ease of use. This will probably change later, as
the structure of the compiler settles and better data structures are chosen for
each task.

The same file also defines implements utility functions for working on lists,
named after their Common Lisp equivalents.

.. literalinclude:: ../../compiler/list.rs
   :language: rust
   :lines: 7-62
   :linenos:

:code:`ast.rs`
==============

:code:`reader.rs`
=================

:code:`modules.rs`
==================

This module takes care of implementing modules, validating user-supplied
definitions, processing those definitions into :code:`Module` data structures,
and adding module prefixes to syntax trees (Known as **modularizing**).

Types and Utilities
-------------------

.. literalinclude:: ../../compiler/modules.rs
   :language: rust
   :lines: 9-24
   :linenos:

Modularizing Source Trees
-------------------------

The functionality is split into three functions: `modularize`, which takes an
AST and the name of the current module, `modularize_atom` which works on a
single atom, and `modularize_atom_value`, which adds a module prefix to the atom
if it's an identifier and doesn't have one already.

.. literalinclude:: ../../compiler/modules.rs
   :language: rust
   :lines: 26-53
   :linenos:

:code:`macros.rs`
=================

:code:`types.rs`
================
