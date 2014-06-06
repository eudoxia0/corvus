# Special Forms

# Language Core

# Type System

## Basic Types

### Integers

Integers can be of any bit-width between 1 (Boolean) and 2<sup>23</sup>-1, but
[type legalization](http://blog.llvm.org/2011/12/llvm-31-vector-changes.html)
will convert integers to the nearest lossless representation (ie, `i12` will
likely become `i32` or `i16`).

* `i1`: Boolean, can only be true and false.
* `i32`: C's `int`.
* `ui64`: C's `unsigned long long`.
* `i128`: A very large integer.

The symbol `word` is aliased to the integer type that represents a machine
word. In 64-bit systems, this is usually `i64`.

### Floating-point

* `half`: Half precision, 16 bits wide.
* `single`: Single precision, 32 bits wide.
* `double`: Double precision, 64 bits wide.
* `quad`: Quadruple precision, 128 bits wide.

## Aggregate Types

### Tuples

### Records

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
