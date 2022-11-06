
.. _sec-invoking:

Invoking Happy
==============

An invocation of Happy has the following syntax:

::

   $ happy [ options ] filename [ options ]

All the command line options are optional (!) and may occur either
before or after the input file name. Options that take arguments may be
given multiple times, and the last occurrence will be the value used.

There are two types of grammar files, ``file.y`` and ``file.ly``, with
the latter observing the reverse comment (or literate) convention (i.e.
each code line must begin with the character ``>``, lines which don't
begin with ``>`` are treated as comments). The examples distributed with
Happy are all of the .ly form.

literate grammar files
The flags accepted by Happy are as follows:

``-o`` <file>; ``--outfile``\ =<file>
   Specifies the destination of the generated parser module. If omitted,
   the parser will be placed in <file>\ ``.hs``, where <file> is the
   name of the input file with any extension removed.

``-i``\ [<file>]; ``--info``\ [=<file>]
   info file
   Directs Happy to produce an info file containing detailed information
   about the grammar, parser states, parser actions, and conflicts. Info
   files are vital during the debugging of grammars. The filename
   argument is optional (note that there's no space between ``-i`` and
   the filename in the short version), and if omitted the info file will
   be written to <file>\ ``.info`` (where <file> is the input file name
   with any extension removed).
``-p``\ [<file>]; ``--pretty``\ [=<file>]
   pretty print
   Directs Happy to produce a file containing a pretty-printed form of
   the grammar, containing only the productions, without any semantic
   actions or type signatures. If no file name is provided, then the
   file name will be computed by replacing the extension of the input
   file with ``.grammar``.
``-t`` <dir>; ``--template``\ =<dir>
   template files
   Instructs Happy to use this directory when looking for template
   files: these files contain the static code that Happy includes in
   every generated parser. You shouldn't need to use this option if
   Happy is properly configured for your computer.
``-m`` <name>; ``--magic-name``\ =<name>
   Happy prefixes all the symbols it uses internally with either
   ``happy`` or ``Happy``. To use a different string, for example if the
   use of ``happy`` is conflicting with one of your own functions,
   specify the prefix using the ``-m`` option.

``-s``; ``--strict``
   NOTE: the ``--strict`` option is experimental and may cause
   unpredictable results.

   This option causes the right hand side of each production (the
   semantic value) to be evaluated eagerly at the moment the production
   is reduced. If the lazy behaviour is not required, then using this
   option will improve performance and may reduce space leaks. Note that
   the parser as a whole is never lazy - the whole input will always be
   consumed before any input is produced, regardless of the setting of
   the ``--strict`` flag.

``-g``; ``--ghc``
   GHC
   back-ends
   GHC
   Instructs Happy to generate a parser that uses GHC-specific
   extensions to obtain faster code.
``-c``; ``--coerce``
   coerce
   back-ends
   coerce
   Use GHC's ``unsafeCoerce#`` extension to generate smaller faster
   parsers. Type-safety isn't compromised.

   This option may only be used in conjunction with ``-g``.
``-a``; ``--arrays``
   arrays
   back-ends
   arrays
   Instructs Happy to generate a parser using an array-based shift
   reduce parser. When used in conjunction with ``-g``, the arrays will
   be encoded as strings, resulting in faster parsers. Without ``-g``,
   standard Haskell arrays will be used.
``-d``; ``--debug``
   debug
   back-ends
   debug
   Generate a parser that will print debugging information to ``stderr``
   at run-time, including all the shifts, reductions, state transitions
   and token inputs performed by the parser.

   This option can only be used in conjunction with ``-a``.
``-l``; ``--glr``
   glr
   back-ends
   glr
   Generate a GLR parser for ambiguous grammars.
``-k``; ``--decode``
   decode
   Generate simple decoding code for GLR result.
``-f``; ``--filter``
   filter
   Filter the GLR parse forest with respect to semantic usage.
``-?``; ``--help``
   Print usage information on standard output then exit successfully.

``-V``; ``--version``
   Print version information on standard output then exit successfully.
   Note that for legacy reasons ``-v`` is supported, too, but the use of
   it is deprecated. ``-v`` will be used for verbose mode when it is
   actually implemented.
