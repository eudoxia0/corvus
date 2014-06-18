*************
Language Core
*************

Mathematical Operations
=======================

Bitwise Operations
==================

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
