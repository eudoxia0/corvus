******
Macros
******

Macros like these:0

::

  (defmacro id
    (x) x)

  (defmacro unless
    (cond tb fb) `(if (not ,cond) ,tb ,fn))

Generates the following anonymous functions:

::

  (lambda ((x SExp)) x)

  (lambda ((cond SExp) (tb SExp) (fb SExp))
    (cons (atom "if")
          (cons (cons (atom "not")
                      (cons cond nil))
                (cons tb (cons fb nil)))))
