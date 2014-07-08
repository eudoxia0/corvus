*******
Systems
*******

System Definition
=================

A system is defined in a :code:`.csd` file, which stands for *Corvus System
Definition*. :code:`.csd` files are kept in the root of the source directory,
and cannot be kept further down. The package manager will only look for files in
the root directory.

A system definition looks like this:

::

  (defsystem myapp
    (authors
      ("Fernando Borretti" "eudoxiahp@gmail.com"))
    (version "1.2.34")
    (license "MIT")
    (tags "os" "network")
    (components
      (dir "src"
        (options serial)
        (file "network")
        (file "file parser"))
      (dir "bin"
        (options (depends-on "src"))
        (file "mmp2pdb")
        (file "pdb2mmp"))))

The first argument to the :code:`defsystem` form is the name of the
system. Typically, this will match the filename (eg :code:`myapp.csd`), but this
is not required. The rest of the arguments are :code:`(<property> <value>)`
lists that are described below.

Properties
----------

:code:`authors`
^^^^^^^^^^^^^^^
Syntax
  * :code:`(authors <author>+)`
  *
    ::

      <author> = <name>
                 (<name> <email> <description>?)
Required?
  Yes

The :code:`authors` is a list of *authors*, where each author is either the
author's name; or a list, where the first element is the author's name, the
second the author's email [#f1]_, and the third optional argument is a
description of the author's role in the project.

.. rubric:: Footnotes

.. [#f1] If the project's version control system uses another similarly
         ubiquitous contact information for each author, that may be used as
         well. This may become useful in a future where email has be superseded
         or abandoned.
