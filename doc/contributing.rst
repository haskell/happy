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

- ``src``: The source code for the Happy executable itself

- ``packages/*``: The various packages that make up Happy behind the scenes, and are available for reuse for other purposes.

- ``doc``: The documentation

  This is in reStructured Text format as is common for many Haskell tools' documentation.
  To build the documentation, use [Sphinx](https://www.sphinx-doc.org/).

- ``examples``: Various examples of using Happy

Build Instructions
------------------

Happy is mostly a normal Cabal-packaged Haskell executable::

    $ cabal build

The only wrinkle is that developing Happy's own parser (i.e. the frontend
component that parses ``.y`` files) requires an existing Happy executable on
the PATH.

Do *not* modify these files by hand::

    packages/frontend/src/Happy/Frontend/Parser.hs
    packages/frontend/src/Happy/Frontend/AttrGrammar/Parser.hs

Instead, edit these files::

    packages/frontend/boot-src/Parser.ly
    packages/frontend/boot-src/AttrGrammarParser.ly

and regenerate the ``.hs``-files with::

    $ packages/frontend/bootstrap.sh
