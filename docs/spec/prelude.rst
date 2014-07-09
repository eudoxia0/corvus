*******
Prelude
*******

The prelude is a standard module that contains various useful constructs defined
within the language.

Kinds
=====

The Prelude contains four built-in kinds: :code:`Integer` and :code:`Float` are
the set of all integer and floating-point numbers (Standard or not)
respectively, :code:`Number` is the set union of :code:`Integer` and
:code:`Float`, :code:`Scalar` is the set union of :code:`Number` and `bool`.

.. literalinclude:: ../lang/prelude.cor
   :language: lisp
   :lines: 11-17
   :linenos:

Hierarchy of Built-in Kinds
---------------------------

.. graphviz:: kind-hierarchy.dot

Option Type
===========

.. literalinclude:: ../../lang/prelude.cor
   :language: lisp
   :lines: 21-25
   :linenos:
