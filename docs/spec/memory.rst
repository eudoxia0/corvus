******
Memory
******

Unsafe Pointer
==============

**Note: This section is pretty speculative and should be ignored for now.**

The unsafe pointer is the only pointer type that may be null. Unsafe pointers
are meant to be used as a bridge between Corvus and C libraries. They are not
meant to be used 'directly': Instead, they should be tested for validity and
casted.

Unsafe pointers support three operations:

* :code:`null?`: Tests whether the pointer is null.
* :code:`safe-cast`: Casts the unsafe pointer to a safe pointer, or an
  array. The second argument must be a block of code to execute if the pointer
  is null.
* :code:`ptrcast`: Convert an unsafe pointer to another kind of unsafe
  pointer. Useful for dealing with functions that return void.

For example:

::

  (let (ptr (some-c-function))
    ;; Unsafe pointers can be tested for nullity
    (if (null? ptr)
        /* Do something here */)
    ;; We can cast pointers to safe pointers
    (safe-cast ptr
      ;; In case it is null:
      (fail "Pointer was null!"))
Note that :code:`safe` itself tests whether the pointer is null and raises a
condition accordingly. The explicit test is to choose what exactly is to be
done: The double test will be optimized out.

The :code:`null` constant
-------------------------

So far we have only seen how to 'purify' pointers that come from the outside. We
can also do the converse: Cast regular pointers to unsafe pointers, or pass the
null pointer to a C functiona that takes it as an argument.

Owned Pointer
=============

Pointers are created by the :code:`new` operation and can be freed. They can
never be null. They may be dereferenced. The memory they reference written to.

Unique Pointer
--------------

Unique pointers, as their name implies, point to some area of memory that only
they may own. They are automatically garbage collected. Trying to copy a unique
pointer somewhere will result in an error. For example, you can't do this:

::

  (let (unique (u 10))
    (some-function unique) ;; Illegal
    (better-function (ref unique)) ;; This is better

Shared Pointer
--------------

Shared pointers implement reference counting.

Reference
---------

References are created by 'borrowing' a pointer through the :code:`ref` form,
and within the lifetime of that reference, nothing can be done to the original
pointer. This is known as freezing. For instance, the following code will
produce a compile-time error:

::

  (let (p (create 10)
        ref (ref p))
    (some-function p) ;; Might mutate the value p points to, thus changing the
                      ;; reference
    (set (load p) 11)) ;; Changes the value p points to

Pointer Hierarchy
-----------------

.. graphviz::

   digraph ptrs {
      raw [label="Raw Pointer (*)"];
      owned [label="Owned Pointer (p)"];
      unique [label="Unique Pointer (u)"];
      shared [label="Shared Pointer (s)"];
      ref [label="Reference (ref)"];
      raw -> owned -> unique;
      owned -> shared;
      owned -> ref;
      unique -> ref;
      shared -> ref;
   }
