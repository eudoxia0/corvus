*******
Systems
*******

For structuring large projects, Corvus contains mechanism for defining
*systems*. A system is a collection of code, along with metadata, dependencies
on other systems, and intructions on how the different components depend on each
other.

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
    (version "1.2.23")
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

Examples:

::

  (authors
    "John Smith"
    ("John Doe" "jdoe@example.com")
    ("Jane Doe" "janed@example.com" "Lead maintainer"))

:code:`version`
^^^^^^^^^^^^^^^
Syntax
  * :code:`(version <version-string>)`
Required?
  Yes

The version is a string that describes the current version of the project. The
string must be a period-separated list of positive integers, ie, satisfying the
regular expression :code:`(\d+\.)+\d+`.

Examples:

::

  (version "0.1")

  (version "1.2.3")

:code:`license`
^^^^^^^^^^^^^^^
Syntax
  * :code:`(license <license-string>)`
Required?
  Yes

The license is a string with the name of the license used by the
project. Additional details, like the full text of the license, may exist
somewhere else in the repository, but this string must hold the name of the
license (ie, not a reference like "See LICENSE.txt").

Examples:

::

  (license "MIT")

  (license "GPLv3")

  (license "Dual MIT and Apache 2.0")

:code:`categories`
^^^^^^^^^^^^^^^^^^
Syntax
  * :code:`(categories (<category-node>+)+)`
Required?
  No

Categories can be used to classify systems into various groups. Each category is
a list of strings from the :ref:`category tree <cat-tree>`. For example:

::

  (categories
    ("net" "p2p")
    ("environment" "console")
    ("sci" "bio")) ;; Note in this line that it is not necessary to specify a
                   ;; "full" category like "synbio"

:code:`tags`
^^^^^^^^^^^^
Syntax
  * :code:`(tags <tag>+)`
Required?
  No

The tags property allows the system developers to categorize their project with
custom tags that are more specific than the supported categories.

Examples:

::

  (tags "kernel" "microkernel" "lisp-machine")

.. rubric:: Footnotes

.. [#f1] If the project's version control system uses another similarly
         ubiquitous contact information for each author, that may be used as
         well. This may become useful in a future where email has be superseded
         or abandoned.

Components
----------

The :code:`components` property of a :code:`defsystem` form contains a list of
*targets*. A target is can be a file, or a directory of other files and targets.

.. _cat-tree:

Categories
==========

* :code:`language`

  * Any ISO 639 code. No edition specified.

* :code:`environment`: The type of environment in which the application runs.

  * :code:`web`
  * :code:`console`
  * :code:`graphical`
  * :code:`daemon`

* :code:`framework`
* :code:`library`
* :code:`application`
* :code:`extension`: Language extensions.
* :code:`net`: Networking.

  * :code:`p2p`: Peer-to-peer networking.
  * :code:`irc`: Internet relay chat.
  * :code:`mail`: E-mail.
  * :code:`im`: Instant messaging.

* :code:`cad`: Computer-aided design.
* :code:`sci`: Science.

  * :code:`bio`: Biology.

    * :code:`synbio`: Synthetic biology.
    * :code:`molbio`: Molecular biology.

  * :code:`astro`: Astronomy.

* :code:`parallel`: Support for parallel computing and HPC.

System Trees
============

A *system tree* is a collection of systems on the filesystem. Both repository
servers and clients hold a tree of systems. It looks roughly like this:

::

   systems/
     system-a
       0.1/
         ...
       0.5/
         ...

That is, different versions of systems are stored each in a folder. The contents
of the :code:`systems/` directory is not required to recognize the addition of a
package immediately.
