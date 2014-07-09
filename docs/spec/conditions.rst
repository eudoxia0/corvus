**********
Conditions
**********

Corvus does not provide exceptions.

Built-in Conditions
===================

Arithmetic Conditions
---------------------

:code:`DivisionByZeroError`
^^^^^^^^^^^^^^^^^^^^^^^^^^^

When the :code:`fast-math` feature is disabled, integer division by zero
raises this condition.

Runtime Conditions
------------------

The following exceptions require an operating system to provide signals and
manage memory.

:code:`MemoryError`
^^^^^^^^^^^^^^^^^^^

Attempting to dereference invalid memory will raise this exception. The
definition of invalid memory is up to the memory manager and operating system.
Additionally, this conditioned is signalled by the `SIGSEGV` standard C
signal. If the operating system does not raise this signal, the behaviour is
undefined.
