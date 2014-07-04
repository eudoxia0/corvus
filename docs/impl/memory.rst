******
Memory
******

Determining Reference Lifetimes
===============================

Lifetime: Slice from the point in the code where the block was first seen, to
the last.

::

  /-- %0 = ref 5
  |   %1 = call fn %0, %..., %...
  \--

If, within the lifetime of a reference, there exists any expression involving
the borrowed pointer, an error is thrown.
