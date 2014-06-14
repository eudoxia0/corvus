# Special Forms

# Language Core

# Type System

## Basic Types

### Integers

Integer types are denoted by the letter 'i' followed by the integer's
bit-width. The following integer types are supported:

- `i1`
- `i8`
- `i16`
- `i32`
- `i64`
- `i128`

The symbol `word` is aliased to the integer type that represents a machine
word. In 64-bit systems, this is usually `i64`.

### Floating-point

* `half`: Half precision, 16 bits wide.
* `single`: Single precision, 32 bits wide.
* `double`: Double precision, 64 bits wide.
* `quad`: Quadruple precision, 128 bits wide.

## Aggregate Types

### Arrays

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
returns a pointer to the beginning of the array, but other forms (Like `resize`
or `size`) can access this hidden metadata or manipulate it.

Because of the design, arrays are compatible with C arrays and can be passed to
external C functions that take an array as a parameter transparently.

### Tuples

### Records

## Pointers

## Type Expressions

Type expressions can be used to construct or operate on types.

### Type Construction

* `(tup t1 t2 ... tn)`: Create a tuple of types `t1 t2 ... tn`.
* `(rec (name1 t1) ... (namen tn))`: Create a record type, where each argument
  is a `name,type` pair.
* `(fn t1 t2 ... tn ret)`: Create a function pointer type. `t1` to `tn` are the
  types of the arguments, and `ret` is the return type.

### Type Operations

* `([p|pp|ppp|pppp] type)`: Increases the indirection level of `type` by the
  number of p's in the expression's name. In C terms, `(pp byte)` is equivalent
  to` char**`.
* `(base type)`: If `type` is a pointer of any indirection (eg, pointer to
  pointer to ...), return the base type.
* `(ret fn-type)`: Extract the return type from a function pointer type. For
  example, `(ret (fn i32 i32 (p i8)))` is `i8`.
