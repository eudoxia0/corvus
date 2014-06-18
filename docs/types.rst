Types
*****

Scalar Types
============

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
---------------

Arrays
^^^^^^

Arrays are a kind of 'fat pointer': They point to a region of data but also
carry length information. This data is hidden before the region they point to:

```
+------+----------+----------------···
| Size | Capacity | Array contents
+------+----------+----------------···
```

Specifically: When `new` creates an array of *n* elements (Where *n* is not
necessarily known at compile time), it allocates enough memory for *n* elements
of the array's type plus two machine word. The first word is used to store
length information, the next one stores the array's capacity. The `new` form
returns a pointer to the beginning of the array, but other forms (Like `realloc`
or `size`) can access this hidden metadata or manipulate it.

Because of the design, arrays are compatible with C arrays and can be passed to
external C functions that take an array as a parameter transparently.
