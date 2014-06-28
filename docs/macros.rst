******
Macros
******

Corvus offers two kinds of macros: *pattern macros*, which map patterns to
templates, and transforming macros, which allow arbitrary manipulation of
S-expressions.

.. _pat_macro:

Pattern Macros
==============

A pattern macro maps a *pattern* to a *template*. Patterns are expressions, like
any other, that may contain variables. Variables are simply identifiers that
begin and end with the character `$`.

Syntax
------

::

  (defsyntax <name>
    [<pattern> <template>]+)

Patterns
--------

Templates
---------

Template Operations
^^^^^^^^^^^^^^^^^^^

Examples
--------

Identity Macro
^^^^^^^^^^^^^^

Definition:

::

  (defsyntax id
    ($x) $x)

Usage:

::

  ;; Input
  (id x)

  ;; Output
  x
