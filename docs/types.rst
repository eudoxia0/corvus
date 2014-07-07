*****
Types
*****

A *type* is a set of objects which share their structural implementation. An
object belongs to a single type. Corvus has no 'type objects': Types may only be
represented by :ref:`type specifiers <type-spec>` and manipulated by certain
special forms.

Unit
====

The unit type, represented by the empty form :code:`()`, is a bottom type. It is
similar in purpose to :code:`void` in C and C-like languages.

Scalar Types
============

Scalar types include integers and floating-point numbers. The symbols in this
section are the :ref:`type specifier <type-spec>` for these types.

Boolean
-------

The single boolean type, :code:`bool`, has two values: `true` and `false`.

Integers
--------

Integer types are denoted by the letter 'i' followed by the integer's
bit-width. The following integer types are supported:

* :code:`i8`
* :code:`i16`
* :code:`i32`
* :code:`i64`
* :code:`i128`

The symbol :code:`word` is aliased to the integer type that represents a machine
word. In 64-bit systems, this is usually :code:`i64`.

Floating-point
--------------

* :code:`single`: Single precision, 32 bits wide.
* :code:`double`: Double precision, 64 bits wide.
* :code:`quad`: Quadruple precision, 128 bits wide.

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

Datatypes, also known as sum types or algebraic data types, are a kind of
"top-down" variant of object-orientation. Instead of defining classes and then
subclasses, the programmer defines a datatype that has multiple variants. For
example, an alternate boolean type may be defined as:

::

  (type Bool (datatype Truth Falsehood))

Datatypes, however, can be used for more than enumerations. For example:

::

  (type Point (tup double double))

  (type Shape
   (datatype
     (Square (bottom-left Point) (upper-right Point))
     (Circle (center Point) (radius double))))


Generics
========

Kinds
=====

Kinds are similar in purpose to the *type classes* in Haskell and *traits* in
Rust. They provide a way to represent bounded polymorphism.

By default, a type variable represents universal quantification. For example, in
the following definition of the identity function:

::

  (defn id ((x ?T)) ?T x)

The type of the argument :code:`x` is a type variable that will work for any
type. In this case, this is precisely what we want, but consider the following:

::

  (defn evenp ((n ?T)) bool
    (= (% n 2) 0))

This function will be specialized for any type of :code:`n`, but it will only
work for those types where the operations are defined. We can use kinds to bound
the set of types that will be accepted by the type variable :code:`?T`. For
example, if we wish to limit this function to the integers, we can use the
built-in kind :code:`Integer`.

::

  (defn evenp ((n (?T Integer))) bool
    (= (% n 2) 0))

Defining Kinds
--------------

Logic Operations
^^^^^^^^^^^^^^^^

Logic operations allow us to create a kind that includes types based on what
they *are*.

For example, some of the basic kinds defined in the Prelude are defined like
this:

.. literalinclude:: ../lang/prelude.cor
   :language: lisp
   :lines: 11-17
   :linenos:


Defined Functions
^^^^^^^^^^^^^^^^^

In contrast to logic operations, this mechanism resembles the type classes of
Haskell more closely, allowing us to create a kind that includes types on the
basis of what operations are allowed on those types.


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

.. _type-spec:

Type Specifiers
===============

A type specifier is an expression that represents a type.

Type Operations
---------------

* :code:`(base <type>)`: If `type` is a pointer of any indirection (eg, pointer
  to pointer to ...), return the base type.
* :code:`(ret <fn-type>)`: Extract the return type from a function pointer
  type. For example, `(ret (fn i32 i32 (p i8)))` is `i8`.
