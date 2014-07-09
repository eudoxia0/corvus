***********
Style Guide
***********

Syntax
======

Indentation
-----------

Indentation is two spaces throughout. When the arguments to functions or special
forms span multiple lines, two approaches are taken:

* **Same-column style**: The :code:`if` special form, and all function calls,
  are indented with each argument on its own line, at the same column as the
  beginning of the first argument. For example:

  ::

    (if (> x 1)
        (something x)
        (something-else x))

    (my-function x
                 y
                 z)

* **First-leads style**: The first argument is on the same line as the name of
  the function or macro, while the rest of the arguments are on other lines,
  indented by *two spaces*. For example:

  ::

    (do obj
      (something)
      (something-else 3.14))

  This approach is common with macros where the first argument (Or arguments)
  specify act as 'options' and the rest of the arguments are a body of code to
  be executed. For example:

  ::

    (w/outfile (stream "file.txt")
      (write stream "hello, world!"))
      

Structuring Projects
====================

Directory Tree
--------------

For standalone applications:

::

  app/
    bin/
      executable-1.cor
      executable-2.cor
    src/
      ...
    t/
      ...
  app.csd
  README.*

For libraries:

::

  app/
    src/
      ...
    t/
      ...
  app.csd
  README.*

Sources
-------

Tests
-----
