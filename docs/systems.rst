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
