**********
Evaluation
**********

Scope
=====

The scoping rules are as follows:

* **Type definitions** may occur anywhere, but are global, and cannot be
  shadowed. Moreover, type definitions are extracted from the form being
  compiled before any other code is actually compiled.
* **Variables** are lexically-scoped, and variables in inner scopes shadow
  variables in outer scopes with the same name.
* **Functions** are also lexically scoped: If two functions with the same name
  and the same argument list are declared, a call in the innermost scope will
  call the innermost function.
