******
Memory
******

Types of Pointers
=================

Unsafe Pointer
--------------

Properties:

* Can be :code:`null` (C's :code:`NULL`).
* Supports two operations: :code:`null?`, which tests whether it is null or not,
  and :code:`safe`, which casts it into a pointer.

Unsafe pointers are meant to be used as a bridge between Corvus and C
libraries. They are not meant to be used 'directly': Instead, they should be
tested for validity. For example:

::

  (if (null? ptr)
      ;; Handle the null pointer case 
      (safe ptr))

Note that :code:`safe` itself tests whether the pointer is null and raises a
condition accordingly. The explicit test is to choose what exactly is to be
done: The double test will be optimized out.

Pointer
-------

Reference
---------
