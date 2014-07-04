*************
Language Core
*************

Mathematical Operations
=======================

+ - * / %

Logic
=====

and or not

Bitwise Operations
==================

Bit-shifting
------------

Syntax
   :code:`(<shl|shr> <argument> <n>)`
Return Value
   `(typeof <argument>)`: The result of the shift.
Parameters
   * :code:`<argument>`: The value to be shifted. Any integer type.
   * :code:`<n>`: The number of bits to :code:`<argument>` argument by.

:code:`shl` shifts :code:`<argument>` *n* bits to the left, :code:`shr` to the
right. In the case of :code:`shr`, whether :code:`<argument>` is a signed or
unsigned integer determines whether this is logical or arithmetic right-shift,
respectively.

Bitwise Logic
-------------

Syntax
   :code:`(bit-<and|or|xor> <a> <b>)`
Return Value
   `(typeof <a>)`: The result of the operation.
Parameters
   * :code:`<a>`, :code:`<b>`: The values to operate on. Must be of the same
     integer type.

:code:`bit-and`, :code:`bit-or` and :code:`bit-xor` do exactly what their names
imply.

Bit Counting
------------

Syntax
   :code:`(<ones|l-ones|t-ones> <argument>)`
Return Value
   `(typeof <argument>)`: The result of the operation.
Parameters
   * :code:`<argument>`: The value to operate on. Must be an integer.

:code:`ones` returns the `Hamming weight`_ of the argument. :code:`l-ones` counts
the number of most significant zeros. :code:`t-ones` counts the number of least
significant zeros.

.. _`Hamming weight`: http://en.wikipedia.org/wiki/Hamming_weight

Tuple and Array Literals
========================

Memory Management
=================

:code:`new`
-----------

Syntax
   :code:`(new <instance> <length?>)`
Return Value
   `(p T)`: A pointer to the heap-allocated `<instance>`.
Parameters
   * :code:`<instance>`: The object to store in the heap.
   * :code:`<length>`: If provided, the length of the array to create with
     copies of `<instance>`.

:code:`realloc`
---------------

Syntax
   :code:`(realloc <array> <length>)`
Return Value
   `(p T)`: A pointer to the new array.
Parameters
   * :code:`<array>`: The array to resize.
   * :code:`<length>`: The new length of the array.

:code:`free`
------------

Syntax
   :code:`(free <pointer>)`
Return Value
   `()`: Unit.
Parameters
   * :code:`<pointer>`: The pointer to deallocate.

Memory
======

ref
address
nth
access

Printing
========

show
