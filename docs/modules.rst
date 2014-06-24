*******
Modules
*******

Module Concepts
===============

Imports
-------

Exports
-------

Inheritance
-----------

Name Conflicts
--------------

Module Interface
================

Defining Modules
----------------

Using Modules
-------------

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

Standard Modules
================

:code:`core`
------------

The core module contains the entire language: Special forms, the language
core, and any global symbols.

:code:`prelude`
---------------

The prelude module contains macros and other definitions that are generally
useful, but are not implemented in the compiler itself.

:code:`main`
------------

The main module is the module that the compiler defaults to. This module
inherits from :code:`core` and :code:`prelude`.

Implementation-Specific Modules
-------------------------------

Other modules may hold implementation-specific functionality. The
implementation is not required to provide it.
