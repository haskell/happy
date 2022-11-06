
.. _sec-glr:

Generalized LR Parsing
======================

This chapter explains how to use the GLR parsing extension, which allows Happy to parse ambiguous grammars and produce useful results.
This extension is triggered with the ``--glr`` flag, which causes Happy to use a different driver for the LALR(1) parsing tables.
The result of parsing is a structure which encodes compactly *all* of the possible parses.
There are two options for how semantic information is combined with the structural information.

This extension was developed by Paul Callaghan and Ben Medlock (University of Durham).
It is based on the structural parser implemented in Medlock's undergraduate project, but significantly extended and improved by Callaghan.
Bug reports, comments, questions etc should be sent to P.C.Callaghan@durham.ac.uk.
Further information can be found on Callaghan's `GLR parser page <http://www.dur.ac.uk/p.c.callaghan/happy-glr>`__.

.. _sec-glr-intro:

Introduction
------------

Here's an ambiguous grammar.
It has no information about the associativity of ``+``, so for example, ``1+2+3`` can be parsed as ``(1+(2+3))`` or ``((1+2)+3)``.
In conventional mode, Happy, would complain about a shift/reduce conflict, although it would generate a parser which always shifts in such a conflict, and hence would produce *only* the first alternative above.

::

   E -> E + E
   E -> i       -- any integer

GLR parsing will accept this grammar without complaint, and produce a result which encodes *both* alternatives simultaneously.
Now consider the more interesting example of ``1+2+3+4``, which has five distinct parses --- try to list them!
You will see that some of the subtrees are identical.
A further property of the GLR output is that such sub-results are shared, hence efficiently represented:
there is no combinatorial explosion.
Below is the simplified output of the GLR parser for this example.

::

   Root (0,7,G_E)
   (0,1,G_E)     => [[(0,1,Tok '1'))]]
   (0,3,G_E)     => [[(0,1,G_E),(1,2,Tok '+'),(2,3,G_E)]]
   (0,5,G_E)     => [[(0,1,G_E),(1,2,Tok '+'),(2,5,G_E)]
                     ,[(0,3,G_E),(3,4,Tok '+'),(4,5,G_E)]]
   (0,7,G_E)     => [[(0,3,G_E),(3,4,Tok '+'),(4,7,G_E)]
                     ,[(0,1,G_E),(1,2,Tok '+'),(2,7,G_E)]
                     ,[(0,5,G_E),(5,6,Tok '+'),(6,7,G_E)]}]
   (2,3,G_E)     => [[(2,3,Tok '2'))]}]
   (2,5,G_E)     => [[(2,3,G_E),(3,4,Tok '+'),(4,5,G_E)]}]
   (2,7,G_E)     => [[(2,3,G_E),(3,4,Tok '+'),(4,7,G_E)]}
                     ,[(2,5,G_E),(5,6,Tok '+'),(6,7,G_E)]}]
   (4,5,G_E)     => [[(4,5,Tok '3'))]}]
   (4,7,G_E)     => [[(4,5,G_E),(5,6,Tok '+'),(6,7,G_E)]}]
   (6,7,G_E)     => [[(6,7,Tok '4'))]}]

This is a directed, acyclic and-or graph.
The node "names" are of form ``(a,b,c)`` where ``a`` and ``b`` are the start and end points (as positions in the input string) and ``c`` is a category (or name of grammar rule).
For example ``(2,7,G_E)`` spans positions 2 to 7 and contains analyses which match the ``E`` grammar rule.
Such analyses are given as a list of alternatives (disjunctions), each corresponding to some use of a production of that category, which in turn are a conjunction of sub-analyses, each represented as a node in the graph or an instance of a token.

Hence ``(2,7,G_E)`` contains two alternatives,
one which has ``(2,3,G_E)`` as its first child and the other with ``(2,5,G_E)`` as its first child,
respectively corresponding to sub-analyses ``(2+(3+4))`` and ``((2+3)+4)``.
Both alternatives have the token ``+`` as their second child, but note that they are difference occurrences of ``+`` in the input!
We strongly recommend looking at such results in graphical form to understand these points.
If you build the ``expr-eval`` example in the directory ``examples/glr``
(N.B. you need to use GHC for this, unless you know how to use the ``-F`` flag for Hugs),
running the example will produce a file which can be viewed with the *daVinci* graph visualization tool.
(See `<http://www.informatik.uni-bremen.de/~davinci/>`__ for more information.
Educational use licenses are currently available without charge.)

The GLR extension also allows semantic information to be attached to productions, as in conventional Happy, although there are further issues to consider.
Two modes are provided, one for simple applications and one for more complex use.
See :ref:`Including semantic results <sec-glr-semantics>`.
The extension is also integrated with Happy's token handling, e.g. extraction of information from tokens.

One key feature of this implementation in Haskell is that its main result is a *graph*.
Other implementations effectively produce a list of trees, but this limits practical use to small examples.
For large and interesting applications,
some of which are discussed in :ref:`Some Applications of GLR parsing <sec-glr-misc-applications>`,
a graph is essential due to the large number of possibilities and the need to analyse the structure of the ambiguity.
Converting the graph to trees could produce huge numbers of results and will lose information about sharing etc.

One final comment.
You may have learnt through using yacc-style tools that ambiguous grammars are to be avoided, and that ambiguity is something that appears only in Natural Language processing.
This is definitely not true.
Many interesting grammars are ambiguous, and with GLR tools they can be used effectively.
We hope you enjoy exploring this fascinating area!

.. _sec-glr-using:

Basic use of a Happy-generated GLR parser
-----------------------------------------

This section explains how to generate and to use a GLR parser to produce structural results.
Please check the examples for further information.
Discussion of semantic issues comes later; see :ref:`Including semantic results <sec-glr-semantics>`.

.. _sec-glr-using-intro:

Overview
~~~~~~~~

The process of generating a GLR parser is broadly the same as for standard Happy.
You write a grammar specification, run Happy on this to generate some Haskell code, then compile and link this into your program.

An alternative to using Happy directly is to use the `BNF Converter <http://www.cs.chalmers.se/~markus/BNFC/>`__ tool by Markus Forsberg, Peter Gammie, Michael Pellauer and Aarne Ranta.
This tool creates an abstract syntax, grammar, pretty-printer and other useful items from a single grammar formalism, thus it saves a lot of work and improves maintainability.
The current output of BNFC can be used with GLR mode now with just a few small changes, but from January 2005 we expect to have a fully-compatible version of BNFC.

Most of the features of Happy still work, but note the important points below.

module header
   The GLR parser is generated in TWO files, one for data and one for the driver.
   This is because the driver code needs to be optimized, but for large parsers with lots of data, optimizing the data tables too causes compilation to be too slow.
   Given a file ``Foo.y``, the file ``FooData.hs``, containing the data module, is generated with basic type information, the parser tables, and the header and tail code that was included in the parser specification.
   Note that Happy can automatically generate the necessary module declaration statements,
   if you do not choose to provide one in the grammar file.
   But, if you do choose to provide the module declaration statement,
   then the name of the module will be parsed and used as the name of the driver module.
   The parsed name will also be used to form the name of the data module,
   but with the string ``Data`` appended to it.
   The driver module, which is to be found in the file ``Foo.hs``,
   will not contain any other user-supplied text besides the module name.
   Do not bother to supply any export declarations in your module declaration statement:
   they will be ignored and dropped, in favor of the standard export declaration.

export of lexer
   You can declare a lexer (and error token) with the ``%lexer``
   directive as normal, but the generated parser does NOT call this
   lexer automatically.
   The action of the directive is only to *export* the lexer function to the top level.
   This is because some applications need finer control of the lexing process.

precedence information
   This still works, but note the reasons.
   The precedence and associativity declarations are used in Happy's LR table creation to resolve certain conflicts.
   It does this by retaining the actions implied by the declarations and removing the ones which clash with these.
   The GLR parser back-end then produces code from these filtered tables, hence the rejected actions are never considered by the GLR parser.

   Hence, declaring precedence and associativity is still a good thing, since it avoids a certain amount of ambiguity that the user knows how to remove.

monad directive
   There is some support for monadic parsers.
   The "tree decoding" mode (see :ref:`Tree decoding <sec-glr-semantics-tree>`) can use the information given in the ``%monad`` declaration to monadify the decoding process.
   This is explained in more detail in :ref:`Monadic tree decoding <sec-glr-semantics-tree-monad>`.

   *Note*: the generated parsers don't include Ashley Yakeley's monad context information yet.
   It is currently just ignored.
   If this is a problem, email and I'll make the changes required.

parser name directive
   This has no effect at present.
   It will probably remain this way:
   if you want to control names, you could use qualified import.

type information on non-terminals
   The generation of semantic code relies on type information given in
   the grammar specification.
   If you don't give an explicit signature, the type ``()`` is assumed.
   If you get type clashes mentioning ``()`` you may need to add type annotations.
   Similarly, if you don't supply code for the semantic rule portion, then the value ``()`` is used.

``error`` symbol in grammars, and recovery
   No attempt to implement this yet.
   Any use of ``error`` in grammars is thus ignored, and parse errors will eventually mean a parse will fail.

the token type
   The type used for tokens *must* be in the ``Ord`` type class (and hence in ``Eq``).
   It is also recommended that they are in the ``Show`` class too.
   The ordering is required for the implementation of ambiguity packing.
   It may be possible to relax this requirement, but it is probably simpler just to require instances of the type classes.
   Please tell us if this is a problem.

.. _sec-glr-using-main:

The main function
~~~~~~~~~~~~~~~~~

The driver file exports a function ``doParse :: [[UserDefTok]] -> GLRResult``.
If you are using several parsers, use qualified naming to distinguish them.
``UserDefTok`` is a synonym for the type declared with the ``%tokentype`` directive.

.. _sec-glr-using-input:

The input
~~~~~~~~~

The input to ``doParse`` is a list of *list of* token values.
The outer level represents the sequence of input symbols, and the inner list represents ambiguity in the tokenisation of each input symbol.
For example, the word "run" can be at least a noun or a verb, hence the inner list will contain at least two values.
If your tokens are not ambiguous, you will need to convert each token to a singleton list before parsing.

.. _sec-glr-using-output:

The Parse Result
~~~~~~~~~~~~~~~~

The parse result is expressed with the following types.
A successful parse yields a forest (explained below) and a single root node for the forest.
A parse may fail for one of two reasons: running out of input or a (global) parse error.
A global parse error means that it was not possible to continue parsing *any* of the live alternatives; this is different from a local error, which simply means that the current alternative dies and we try some other alternative.
In both error cases, the forest at failure point is returned, since it may contain useful information.
Unconsumed tokens are returned when there is a global parse error.

::

   type ForestId = (Int,Int,GSymbol)
   data GSymbol  = <... automatically generated ...>
   type Forest   = FiniteMap ForestId [Branch]
   type RootNode = ForestId
   type Tokens   = [[(Int, GSymbol)]]
   data Branch   = Branch {b_sem :: GSem, b_nodes :: [ForestId]}
   data GSem     = <... automatically generated ...>

   data GLRResult
     = ParseOK     RootNode Forest    -- forest with root
     | ParseError  Tokens   Forest    -- partial forest with bad input
     | ParseEOF             Forest    -- partial forest (missing input)

Conceptually, the parse forest is a directed, acyclic and-or graph.
It is represented by a mapping of ``ForestId``\ s to lists of possible analyses.
The ``FiniteMap`` type is used to provide efficient and convenient access.
The ``ForestId`` type identifies nodes in the graph, named by the range of input they span and the category of analysis they license.
``GSymbol`` is generated automatically as a union of the names of grammar rules (prefixed by ``G_`` to avoid name clashes) and of tokens and an EOF symbol.
Tokens are wrapped in the constructor ``HappyTok :: UserDefTok -> GSymbol``.

The ``Branch`` type represents a match for some right-hand side of a production, containing semantic information (see below) and a list of sub-analyses.
Each of these is a node in the graph.
Note that tokens are represented as childless nodes that span one input position.
Empty productions will appear as childless nodes that start and end at the same position.

.. _sec-glr-using-compiling:

Compiling the parser
~~~~~~~~~~~~~~~~~~~~

Happy will generate two files, and these should be compiled as normal Haskell files.
If speed is an issue, then you should use the ``-O`` flags etc with the driver code, and if feasible, with the parser tables too.

You can also use the ``--ghc`` flag to trigger certain GHC-specific optimizations.
At present, this just causes use of unboxed types in the tables and in some key code.
Using this flag causes relevant GHC option pragmas to be inserted into the generated code, so you shouldn't have to use any strange flags (unless you want to...).

.. _sec-glr-semantics:

Including semantic results
--------------------------

This section discusses the options for including semantic information in grammars.

.. _sec-glr-semantics-intro:

Forms of semantics
~~~~~~~~~~~~~~~~~~

Semantic information may be attached to productions in the conventional way, but when more than one analysis is possible, the use of the semantic information must change.
Two schemes have been implemented, which we call *tree decoding* and *label decoding*.
The former is for simple applications, where there is not much ambiguity and hence where the effective unpacking of the parse forest isn't a factor.
This mode is quite similar to the standard mode in Happy.
The latter is for serious applications, where sharing is important and where processing of the forest (eg filtering) is needed.
Here, the emphasis is about providing rich labels in nodes of the the parse forest, to support such processing.

The default mode is labelling.
If you want the tree decode mode, use the ``--decode`` flag.

.. _sec-glr-semantics-tree:

Tree decoding
~~~~~~~~~~~~~

Tree decoding corresponds to unpacking the parse forest to individual trees and collecting the list of semantic results computed from each of these.
It is a mode intended for simple applications, where there is limited ambiguity.
You may access semantic results from components of a reduction using the dollar variables.
As a working example, the following is taken from the ``expr-tree`` grammar in the examples.
Note that the type signature is required, else the types in use can't be determined by the parser generator.

::

   E :: {Int} -- type signature needed
     : E '+' E  { $1 + $3 }
     | E '*' E  { $1 * $3 }
     | i        { $1 }

This mode works by converting each of the semantic rules into functions (abstracted over the dollar variables mentioned), and labelling each ``Branch`` created from a reduction of that rule with the function value.
This amounts to *delaying* the action of the rule, since we must wait until we know the results of all of the sub-analyses before computing any of the results.
(Certain cases of packing can add new analyses at a later stage.)

At the end of parsing, the functions are applied across relevant sub-analyses via a recursive descent.
The main interface to this is via the class and entry function below.
Typically, ``decode`` should be called on the root of the forest, also supplying a function which maps node names to their list of analyses (typically a partial application of lookup in the forest value).
The result is a list of semantic values.
Note that the context of the call to ``decode`` should (eventually) supply a concrete type to allow selection of appropriate instance.
I.e., you have to indicate in some way what type the semantic result should have.
``Decode_Result a`` is a synonym generated by Happy:
for non-monadic semantics, it is equivalent to ``a``; when monads are in use, it becomes the declared monad type.
See the full ``expr-eval`` example for more information.

::

   class TreeDecode a where
           decode_b :: (ForestId -> [Branch]) -> Branch -> [Decode_Result a]
   decode :: TreeDecode a => (ForestId -> [Branch]) -> ForestId -> [Decode_Result a]

The GLR parser generator identifies the types involved in each semantic rule, hence the types of the functions, then creates a union containing distinct types.
Values of this union are stored in the branches.
(The union is actually a bit more complex:
it must also distinguish patterns of dollar-variable usage, eg a function ``\x y -> x + y`` could be applied to the first and second constituents, or to the first and third.)
The parser generator also creates instances of the ``TreeDecode`` class, which unpacks the semantic function and applies it across the decodings of the possible combinations of children.
Effectively, it does a Cartesian product operation across the lists of semantic results from each of the children.
Eg ``[1,2] "+" [3,4]`` produces ``[4,5,5,6]``.
Information is extracted from token values using the patterns supplied by the user when declaring tokens and their Haskell representation, so the dollar-dollar convention works also.

The decoding process could be made more efficient by using memoisation techniques, but this hasn't been implemented since we believe the other (label) decoding mode is more useful.
(If someone sends in a patch, we may include it in a future release --- but this might be tricky, e.g. require higher-order polymorphism?
Plus, are there other ways of using this form of semantic function?)

.. _sec-glr-semantics-label:

Label decoding
~~~~~~~~~~~~~~

The labelling mode aims to label branches in the forest with information that supports subsequent processing, for example the filtering and prioritisation of analyses prior to extraction of favoured solutions.
As above, code fragments are given in braces and can contain dollar-variables.
But these variables are expanded to node names in the graph, with the intention of easing navigation.
The following grammar is from the ``expr-tree`` example.

::

   E :: {Tree ForestId Int}
     : E '+' E      { Plus  $1 $3 }
     | E '*' E      { Times $1 $3 }
     | i            { Const $1 }

Here, the semantic values provide more meaningful labels than the plain structural information.
In particular, only the interesting parts of the branch are represented, and the programmer can clearly select or label the useful constituents if required.
There is no need to remember that it is the first and third child in the branch which we need to extract, because the label only contains those values (the \`noise' has been dropped).
Consider also the difference between concrete and abstract syntax.
The labels are oriented towards abstract syntax.
Tokens are handled slightly differently here:
when they appear as children in a reduction, their informational content can be extracted directly,
hence the ``Const`` value above will be built with the ``Int`` value from the token, not some ``ForestId``.

Note the useful technique of making the label types polymorphic in the position used for forest indices.
This allows replacement at a later stage with more appropriate values, e.g. inserting lists of actual subtrees from the final decoding.

Use of these labels is supported by a type class ``LabelDecode``, which unpacks values of the automatically-generated union type ``GSem`` to the original type(s).
The parser generator will create appropriate instances of this class, based on the type information in the grammar file.
(Note that omitting type information leads to a default of ``()``.)
Observe that use of the labels is often like traversing an abstract syntax, and the structure of the abstract syntax type usually constrains the types of constituents;
so once the overall type is fixed (e.g. with a type cast or signature) then there are no problems with resolution of class instances.

::

   class LabelDecode a where
           unpack :: GSem -> a

Internally, the semantic values are packed in a union type as before, but there is no direct abstraction step.
Instead, the ``ForestId`` values (from the dollar-variables) are bound when the corresponding branch is created from the list of constituent nodes.
At this stage, token information is also extracted, using the patterns supplied by the user when declaring the tokens.

.. _sec-glr-semantics-tree-monad:

Monadic tree decoding
~~~~~~~~~~~~~~~~~~~~~

You can use the ``%monad`` directive in the tree-decode mode.
Essentially, the decoding process now creates a list of monadic values,
using the monad type declared in the directive.
The default handling of the semantic functions is to apply the relevant ``return`` function to the value being returned.
You can over-ride this using the ``{% ... }`` convention.
The declared ``(>>=)`` function is used to assemble the computations.

Note that no attempt is made to share the results of monadic computations from sub-trees. (You could possibly do this by supplying a memoising lookup function for the decoding process.)
Hence, the usual behaviour is that decoding produces whole monadic computations, each part of which is computed afresh (in depth-first order) when the whole is computed.
Hence you should take care to initialise any relevant state before computing the results from multiple solutions.

This facility is experimental, and we welcome comments or observations on the approach taken! An example is provided (``examples/glr/expr-monad``).
It is the standard example of arithmetic expressions, except that the ``IO`` monad is used, and a user exception is thrown when the second argument to addition is an odd number.
Running this example will show a zero (from the exception handler) instead of the expected number amongst the results from the other parses.

.. _sec-glr-misc:

Further information -------------------

Other useful information...

.. _sec-glr-misc-examples:

The GLR examples ~~~~~~~~~~~~~~~~

The directory ``examples/glr`` contains several examples from the small to the large.
Please consult these or use them as a base for your experiments.

.. _sec-glr-misc-graphs:

Viewing forests as graphs
~~~~~~~~~~~~~~~~~~~~~~~~~

If you run the examples with GHC, each run will produce a file ``out.daVinci``.
This is a graph in the format expected by the *daVinci* graph visualization tool.
(See `<http://www.informatik.uni-bremen.de/~davinci/>`__ for more information.
Educational use licenses are currently available without charge.)

We highly recommend looking at graphs of parse results --- it really helps to understand the results.
The graphs files are created with Sven Panne's library for communicating with *daVinci*, supplemented with some extensions due to Callaghan.
Copies of this code are included in the examples directory, for convenience.
If you are trying to view large and complex graphs, contact Paul Callaghan (there are tools and techniques to make the graphs more manageable).

.. _sec-glr-misc-applications:

Some Applications of GLR parsing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GLR parsing (and related techniques) aren't just for badly written grammars or for things like natural language (NL) where ambiguity is inescapable.
There are applications where ambiguity can represent possible alternatives in pattern-matching tasks, and the flexibility of these parsing techniques and the resulting graphs support deep analyses.
Below, we briefly discuss some examples, a mixture from our recent work and from the literature.

Gene sequence analysis
   Combinations of structures within gene sequences can be expressed as a grammar, for example a "start" combination followed by a "promoter" combination then the gene proper.
   A recent undergraduate project has used this GLR implementation to detect candiate matches in data, and then to filter these matches with a mixture of local and global information.

Rhythmic structure in poetry
   Rhythmic patterns in (English) poetry obey certain rules, and in more modern poetry can break rules in particular ways to achieve certain effects.
   The standard rhythmic patterns (e.g. iambic pentameter) can be encoded as a grammar, and deviations from the patterns also encoded as rules.
   The neutral reading can be parsed with this grammar, to give a forest of alternative matches.
   The forest can be analysed to give a preferred reading, and to highlight certain technical features of the poetry.
   An undergraduate project in Durham has used this implementation for this purpose, with promising results.

Compilers --- instruction selection
   Recent work has phrased the translation problem in compilers from intermediate representation to an instruction set for a given processor as a matching problem.
   Different constructs at the intermediate level can map to several combinations of machine instructions.
   This knowledge can be expressed as a grammar, and instances of the problem solved by parsing.
   The parse forest represents competing solutions, and allows selection of optimum solutions according to various measures.

Robust parsing of ill-formed input
   The extra flexibility of GLR parsing can simplify parsing of formal languages where a degree of \`informality' is allowed.
   For example, Html parsing.
   Modern browsers contain complex parsers which are designed to try to extract useful information from Html text which doesn't follow the rules precisely, eg missing start tags or missing end tags.
   Html with missing tags can be written as an ambiguous grammar, and it should be a simple matter to extract a usable interpretation from a forest of parses.
   Notice the technique: we widen the scope of the grammar, parse with GLR, then extract a reasonable solution.
   This is arguably simpler than pushing an LR(1) or LL(1) parser past its limits, and also more maintainable.

Natural Language Processing
   Ambiguity is inescapable in the syntax of most human languages.
   In realistic systems, parse forests are useful to encode competing analyses in an efficient way, and they also provide a framework for further analysis and disambiguation.
   Note that ambiguity can have many forms, from simple phrase attachment uncertainty to more subtle forms involving mixtures of word senses.
   If some degree of ungrammaticality is to be tolerated in a system, which can be done by extending the grammar with productions incorporating common forms of infelicity, the degree of ambiguity increases further.
   For systems used on arbitrary text, such as on newspapers, it is not uncommon that many sentences permit several hundred or more analyses.
   With such grammars, parse forest techniques are essential.
   Many recent NLP systems use such techniques, including the Durham's earlier LOLITA system - which was mostly written in Haskell.

.. _sec-glr-misc-workings:

Technical details
~~~~~~~~~~~~~~~~~

The original implementation was developed by Ben Medlock, as his undergraduate final year project, using ideas from Peter Ljunglöf's Licentiate thesis
(see `<https://gup.ub.gu.se/publication/10783>`__, and we recommend the thesis for its clear analysis of parsing algorithms).
Ljunglöf's version produces lists of parse trees, but Medlock adapted this to produce an explicit graph containing parse structure information.
He also incorporated the code into Happy.

After Medlock's graduation, Callaghan extended the code to incorporate semantic information,
and made several improvements to the original code,
such as improved local packing and support for hidden left recursion.
The performance of the code was significantly improved, after changes of representation (eg to a chart-style data structure) and technique.
Medlock's code was also used in several student projects, including analysis of gene sequences (Fischer) and analysis of rhythmic patterns in poetry (Henderson).

The current code implements the standard GLR algorithm extended to handle hidden left recursion.
Such recursion, as in the grammar below from Rekers [1992], causes the standard algorithm to loop because the empty reduction ``A ->`` is always possible and the LR parser will not change state.
Alternatively, there is a problem because an unknown (at the start of parsing) number of ``A`` items are required, to match the number of ``i`` tokens in the input.

::

   S -> A Q i | +
   A ->

The solution to this is not surprising.
Problematic recursions are detected as zero-span reductions in a state which has a ``goto`` table entry looping to itself.
A special symbol is pushed to the stack on the first such reduction, and such reductions are done at most once for any token alternative for any input position.
When popping from the stack, if the last token being popped is such a special symbol, then two stack tails are returned:
one corresponding to a conventional pop (which removes the symbol) and the other to a duplication of the special symbol (the stack is not changed, but a copy of the symbol is returned).
This allows sufficient copies of the empty symbol to appear on some stack, hence allowing the parse to complete.

The forest is held in a chart-style data structure, and this supports local ambiguity packing (chart parsing is discussed in Ljunglöf's thesis, among other places).
A limited amount of packing of live stacks is also done, to avoid some repetition of work.

[Rekers 1992] Parser Generation for Interactive Environments, PhD thesis, University of Amsterdam, 1992.

.. _sec-glr-misc-filter:

The ``--filter`` option
~~~~~~~~~~~~~~~~~~~~~~~

You might have noticed this GLR-related option.
It is an experimental feature intended to restrict the amount of structure retained in the forest by discarding everything not required for the semantic results.
It may or it may not work, and may be fixed in a future release.

.. _sec-glr-misc-limitations:

Limitations and future work
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The parser supports hidden left recursion, but makes no attempt to handle cyclic grammars that have rules which do not consume any input.
If you have a grammar like this, for example with rules like ``S -> S`` or ``S -> A S | x; A -> empty``, the implementation will loop until you run out of stack - but if it will happen, it often happens quite quickly!

The code has been used and tested frequently over the past few years, including being used in several undergraduate projects.
It should be fairly stable, but as usual, can't be guaranteed bug-free.
One day I will write it in Epigram!

If you have suggestions for improvements, or requests for features, please contact Paul Callaghan.
There are some changes I am considering, and some views and/or encouragement from users will be much appreciated.
Further information can be found on Callaghan's `GLR parser page <http://www.dur.ac.uk/p.c.callaghan/happy-glr>`__.

.. _sec-glr-misc-acknowledgements:

Thanks and acknowledgements
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many thanks to the people who have used and tested this software in its various forms, including Julia Fischer, James Henderson, and Aarne Ranta.
