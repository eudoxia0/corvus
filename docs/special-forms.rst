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

Syntax
   :code:`(if <cond> <true-branch> <false-branch>)`
Return Value
   `i1`: The value of the chosen branch.
Parameters
   * :code:`<cond>`: The condition to test. Type: `i1`.
   * :code:`<true-branch>`, `<false-branch`>: The condition branches. Must be of
     the same type.

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

Syntax
   :code:`(type <name> <specifier> <docstring?>)`
Return Value
   `i1`: Truth constant.
Parameters
   * :code:`<name>`: The name of the type to define. This must be unique in the
     present scope, otherwise, a duplicate named type error is signalled.
   * :code:`<specifier>`: A type specifier.
   * :code:`<docstring>`: An optional documentation string describing the type.


Memory Management
=================

`new`
-----

Syntax
   :code:`(new <instance> <length?>)`
Return Value
   `(p T)`: A pointer to the heap-allocated `<instance>`.
Parameters
   * :code:`<instance>`: The object to store in the heap.
   * :code:`<length>`: If provided, the length of the array to create with
     copies of `<instance>`.

`realloc`
---------

Syntax
   :code:`(realloc <array> <length>)`
Return Value
   `(p T)`: A pointer to the new array.
Parameters
   * :code:`<array>`: The array to resize.
   * :code:`<length>`: The new length of the array.

`free`
------

Syntax
   :code:`(free <pointer>)`
Return Value
   `i1`: The truth constant.
Parameters
   * :code:`<pointer>`: The pointer to deallocate.

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
