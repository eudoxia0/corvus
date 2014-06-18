# Special Forms

Special forms, also known as operators, are expressions which, as their name
implies, have special execution rules. The simplest example is `if`, which
evaluates its condition before anything else, the evaluates either branch
depending on its value.

## Flow Control

### `if`

Syntax: `(if <cond> <true-branch> <false-branch>)`

`<cond>` must be of type `i1`. Both branches must be of the same type.

## Functions

`defn`
`lambda`

## Assignment and Mutation

`set`
`let`

## Types

## `type`

Syntax: `(type <name> <specifier> <docstring?>)`.

## Memory Management

`new`
`realloc`
`free`

## Macros

`defmacro`

## Conditions

`defcondition`
`handling`

## Compiler

`def-feature`
`feature?`

# Language Core

## Mathematical Operations

## Bitwise Operations

`shl`: Shift the first argument *n* bits to the left, where n is the second
argument.
`shr`: Shift the first argument *n* bits to the right, where *n* is the second
argument. Whether the first argument is a signed or unsigned integer determines
whether this is logical or arithmetic right-shift, respectively.
`bit-and`: Bitwise AND.
`bit-or`: Bitwise OR.
`bit-xor`. Bitwise XOR.

`ones`: Return the [Hamming weight](http://en.wikipedia.org/wiki/Hamming_weight)
of the first argument.
`l-ones`: Count the number of most significant zeros.
`t-ones`: Count the number of least significant zeros.

# Error Handling

## Algebraic Data Types

## Condition System

## Failure
