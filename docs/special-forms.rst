*************
Special Forms
*************

Special forms, also known as operators, are expressions which, as their name
implies, have special execution rules. The simplest example is `if`, which
evaluates its condition before anything else, the evaluates either branch
depending on its value.

Flow Control
============

:code:`if`
----------

Syntax
   :code:`(if <cond> <true-branch> <false-branch>)`
Return Value
   The value of the chosen branch.
Parameters
   * :code:`<cond>`: The condition to test. Type: `bool`.
   * :code:`<true-branch>`, `<false-branch`>: The condition branches. Must be of
     the same type.

Functions
=========

:code:`defn`
------------

Syntax
   :code:`(defn name (<arguments>) <ret> <docstring>? <body>+)`
Return Value
   `()`: Unit.
Parameters
   * :code:`<name>`: The function's name.
   * :code:`<arguments>`: A :ref:`generic argument list <concrete_arglist>`.
   * :code:`<ret>`: A type specifier of the function's return type.
   * :code:`<docstring>`: An optional string documenting what the function does.
   * :code:`<body>`: A sequence of expressions.

:code:`lambda`
--------------

Syntax
   :code:`(lambda (<arguments>) <body>+)`
Return Value
   `(fn <arguments> <ret>)`: A pointer to the function.
Parameters
   * :code:`<arguments>`: A :ref:`concrete argument list <concrete_arglist>`.
   * :code:`<body>`: A sequence of expressions.

Assignment and Mutation
=======================

:code:`let`
-----------

Syntax
   :code:`(let (<bindings>*) <body>+)`
Return Value
   The value of the last expression in :code:`body`.
Parameters
   * :code:`<bindings>`: Each binding is a pair with a name and a value. Unlike
     Common Lisp, bindings don't have to be parenthesized. As such, the number
     of bindings must be even.

Examples:

::

  (let (x 1
        y 3.14)
    (show x))

:code:`set`
-----------

Syntax
   :code:`(set <place> <value>)`
Return Value
   `(typeof <value>)`: Returns :code:`<value>`.
Parameters
   * :code:`<place>`: A set-able value of any type.
   * :code:`<value>`: A value, of the same type as :code:`<place>`.

Types
=====

:code:`type`
------------

Syntax
   :code:`(type <name> <specifier> <docstring?>)`
Return Value
   `()`: Unit.
Parameters
   * :code:`<name>`: The name of the type to define. This must be unique in the
     present scope, otherwise, a duplicate named type error is signalled.
   * :code:`<specifier>`: A type specifier.
   * :code:`<docstring>`: An optional documentation string describing the type.

Examples:

::

  (type bigint i128)

  (type color
    (rec (r i8) (g i8) (b i8)))

  (type point (tup double double double))

:code:`variant?`
----------------

Syntax
   :code:`(variant? <obj> <variant-name>)`
Return Value
   :code:`bool`: Whether the variant of :code:`<obj>` is :code:`<variant-name>`.
Parameters
   * :code:`<obj>`: An instance of an algebraic data type.
   * :code:`<variant-name>`: The name of a variant member of the ADT.

Accessing Type Fields
=====================

:code:`get-variant`
-------------------

Syntax
   :code:`(get-variant <obj> <variant-name> <field-name>)`
Return Value
   The value of the object's variant.
Parameters
   * :code:`<obj>`: An instance of a datatype.
   * :code:`<variant-name>`: The name of the variant.
   * :code:`<field-name>`: The name of the field to access.


Macros
======

:code:`defsyntax`
-----------------

Syntax
   :code:`(defsyntax <name> [<case> <template>]+)`
Return Value
   `()`: Unit.
Parameters
   * :code:`<name>`: The macro name.
   * :code:`<case>`, :code:`<template>`: Each pair maps a pattern in the source
     to a particular template.

Defines a pattern macro. For a complete description, see the :ref:`appropriate
section <pat_macro>`.

Conditions
==========

:code:`defcondition`
--------------------

:code:`handling`
----------------

Compiler API, Reflection
========================

:code:`feature?`
----------------

Syntax
   :code:`(feature? <feature>)`
Return Value
   `bool`: Whether :code:`<feature>` is present in the features list.
Parameters
   * :code:`<feature>`: A symbol to look up in the feature list.

:code:`disassemble`
-------------------

Syntax
   :code:`(disassemble <fn>)`
Return Value
   `()`: Unit.
Parameters
   * :code:`<fn>`: A function.

Print the internal representation of the function :code:`<fn>` to the standard
output stream.

Foreign Function Interface
==========================

:code:`link`
:code:`foreign`
