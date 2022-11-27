.. _sec-obtaining:

Obtaining Happy
===============

.. highlight:: bash

If you just want to *use* Happy, you can build from a release.
This should work the same as any other Haskell package.

Happy itself and its examples are intended to work with GHC >= 7.0.

Haskell-specific way
--------------------

From `Hackage <https://hackage.haskell.org/package/happy>`__ via `Cabal Install <https://www.haskell.org/cabal/>`__::

   $ cabal install happy

From `Stackage <https://www.stackage.org/package/happy>`__ via `Stack <https://haskellstack.org>`__::

   $ stack install happy

Moreover, ``cabal`` will automatically install the required version of ``happy`` based on ``build-tools``/``build-tool-depends`` `declarations <http://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-build-tool-depends>`__.

Operating System way
--------------------

Because Happy is a dependency of GHC, it is often packaged by operating systems.
`Repology <https://repology.org>`__ aggregates this info across many distros and operating systems, and Happy is actually listed twice:

- https://repology.org/project/haskell:happy/versions
- https://repology.org/project/happy/versions

The table contains links to the individual OS packages, which should provide installation instructions.
