.. _contributing:

Contributing to Happy
=====================

.. highlight:: bash

Source Code Repository
----------------------

Happy is hosted on `GitHub <https://github.com/haskell/happy>`__.
As previously discussed in `bug-reports`_, we use the built-in `GitHub issue tracker <https://github.com/haskell/happy/issues>`__ for Happy.
We also use `GitHub pull requests <https://github.com/haskell/happy/pulls>`__ for managing changes;
feel free to submit them!

Repo Layout
-----------

- ``app``: The source code for the Happy executable itself

- ``lib/*``: The various internal libraries that make up the ``happy-lib``
  package. This library is used to implement the ``happy`` executable behind the
  scenes, and is available for reuse for other purposes.

- ``doc``: The documentation

  This is in reStructured Text format as is common for many Haskell tools' documentation.
  To build the documentation, use [Sphinx](https://www.sphinx-doc.org/).

- ``examples``: Various examples of using Happy

Build Instructions
------------------

Happy is mostly a normal Cabal-packaged Haskell executable::

    $ cabal build

The only wrinkle is that changing Happy's own parser (i.e. the frontend
component that parses ``.y`` files) requires an existing Happy executable on
the PATH to run ``lib/frontend/boostrap.sh``.

Do *not* modify these files by hand::

    lib/frontend/src/Happy/Frontend/Parser.hs
    lib/frontend/src/Happy/Frontend/AttrGrammar/Parser.hs

Instead, edit these files::

    lib/frontend/boot-src/Parser.ly
    lib/frontend/boot-src/AttrGrammarParser.ly

and regenerate the ``.hs``-files with::

    $ lib/frontend/bootstrap.sh
