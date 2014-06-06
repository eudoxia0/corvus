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
