*************
Special Forms
*************

Special forms, also known as operators, are expressions which, as their name
implies, have special execution rules. The simplest example is `if`, which
evaluates its condition before anything else, the evaluates either branch
depending on its value.

Flow Control
============

`if`
----

Syntax: `(if <cond> <true-branch> <false-branch>)`

`<cond>` must be of type `i1`. Both branches must be of the same type.

Functions
=========

`defn`
------

`lambda`
--------

Assignment and Mutation
=======================

`set`
-----

`let`
-----

Types
=====

`type`
------

Syntax: `(type <name> <specifier> <docstring?>)`

Memory Management
=================

`new`
-----

`realloc`
---------

`free`
------

Macros
======

`defmacro`
----------

Conditions
==========

`defcondition`
--------------

`handling`
----------

Compiler API, Reflection
========================

`feature?`
----------
