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

Happy is normal Cabal-packaged Haskell executable, except for the fact that a
pre-built Happy is *required* to build the full version of Happy, which is the default.

- If you *do* have an existing Happy executable on the PATH or in the default
  installation location (`~/.cabal/bin` for example), do regular

  ::

    $ cabal build

  like with any other project.

- If you do *not* have an existing Happy executable, instead do

  ::

    $ cabal build -f -bootstrap

- If you install that minimial, non-bootstrapped happy

  ::

    $ cabal install -f -bootstrap

  you can then build normally (with the bootstrap flag enabled).

*We're sorry the bootstrap process is a bit tedious right now; we hope to
improve it in the future. The ideal fix would be to make cabal-installer's
cycle detector to be less pessimistic, per
`<https://github.com/haskell/cabal/issues/7189>`_, so that the build tool dependency
can be properly expressed and everything works automatically.*
