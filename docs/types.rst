*****
Types
*****

Scalar Types
============

Scalar types include integers and floating-point numbers. The symbols in this
section are the type specifiers for these types.

Integers
--------

Integer types are denoted by the letter 'i' followed by the integer's
bit-width. The following integer types are supported:

* `i1`
* `i8`
* `i16`
* `i32`
* `i64`
* `i128`

The symbol `word` is aliased to the integer type that represents a machine
word. In 64-bit systems, this is usually `i64`.

Floating-point
--------------

* `half`: Half precision, 16 bits wide.
* `single`: Single precision, 32 bits wide.
* `double`: Double precision, 64 bits wide.
* `quad`: Quadruple precision, 128 bits wide.

Aggregate Types
===============

Arrays
------

Arrays are a kind of 'fat pointer': They point to a region of data but also
carry length information. This data is hidden before the region they point to::

   +------+----------+----------------···
   | Size | Capacity | Array contents
   +------+----------+----------------···

Specifically: When `new` creates an array of *n* elements (Where *n* is not
necessarily known at compile time), it allocates enough memory for *n* elements
of the array's type plus two machine word. The first word is used to store
length information, the next one stores the array's capacity. The `new` form
returns a pointer to the beginning of the array, but other forms (Like `realloc`
or `size`) can access this hidden metadata or manipulate it.

Because of the design, arrays are compatible with C arrays and can be passed to
external C functions that take an array as a parameter transparently.

Type specifier
   :code:`(arr <type>)`
Examples
   * :code:`(arr i32)` defines an array of 32-bit integers.

Tuples
------

A tuple is an ordered, heterogenous, fixed-size collection of objects.

Type specifier
   :code:`(tup <type>+)`
Examples
   * :code:`(tup i8 i8 i8)` defines a tuple of bytes.

Records
-------

Type specifier
   :code:`(rec (<name> <type>)+)`
Examples
   * :code:`(rec (age i8) (id i64))` defines a record of two fields:
     :code:`age`, which is a byte, and :code:`id`, a 64-bit integer/

Datatypes
=========

Generics
========

Kinds
=====

Functions
=========

Type specifier
   :code:`(fn <arg type>+ <ret type>)`
Examples
   * :code:`(fn i32 i32 double)` defines a function that takes two integers and
     returns a double.

Bounded Polymorphism
--------------------

Pointers
========

Type specifier
   :code:`(<p|pp|ppp|pppp> <type>)`
Examples
   * :code:`(p i8)` defines a pointer to a byte.
   * :code:`(pp double)` defines a pointer to a pointer to a double.

Type Specifiers
===============

A type specifier is an expression that represents a type.

Type Operations
---------------

* `(base type)`: If `type` is a pointer of any indirection (eg, pointer to
  pointer to ...), return the base type.
* `(ret fn-type)`: Extract the return type from a function pointer type. For
  example, `(ret (fn i32 i32 (p i8)))` is `i8`.
