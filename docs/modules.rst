*******
Modules
*******

Module Concepts
===============

Modules are a collection of symbols, some internal, and some external. *Modules
are not files*, nor are they created implicitly like in Python. Modules are
defined using a special form. Modules can export symbols for use by other
modules, and they can import symbols that are external from other modules, or
inherit modules (Import all their external symbols).

Module Interface
================

Modules are defined with the :code:`defmod` form:

::

  (defmod <name> (<parent-modules>*)
    (import <module> <symbols>+)*
    (export <symbols>+)?)`

Using Modules
-------------

The :code:`w/mod` (*With module*) form tells the compiler to enter the given
module. All symbols that are defined are looked in in that module.

Examples
--------

::

  (defmod list (main)
    (export list
            cons
            first
            rest
            append))

  (w/mod list)

  (type list
    (struct ...))

In the above example, the true names of the symbols :code:`type` and
:code:`list` are :code:`core` and :code:`list`, respectively.

Imports
-------

Exports
-------

Inheritance
-----------

Name Conflicts
--------------



Standard Modules
================

:code:`corvus`
--------------

The core module contains the entire language: Special forms, the language
core, and any global symbols.

:code:`prelude`
---------------

The prelude module contains macros and other definitions that are generally
useful, but are not implemented in the compiler itself.

:code:`main`
------------

The main module is the module that the compiler defaults to. This module
inherits from :code:`corvus` and :code:`prelude`.

Implementation-Specific Modules
-------------------------------

Other modules may hold implementation-specific functionality. The
implementation is not required to provide it.
