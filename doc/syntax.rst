
.. _sec-grammar-files:

Syntax of Grammar Files
=======================

The input to Happy is a text file containing the grammar of the language you want to parse,
together with some annotations that help the parser generator make a legal Haskell module that can be included in your program.
This section gives the exact syntax of grammar files.

The overall format of the grammar file is given below:

::

   <optional module header>
   <directives>
   %%
   <grammar>
   <optional module trailer>

.. index::
  single: module; header
  single: module; trailer

If the name of the grammar file ends in ``.ly``, then it is assumed to be a literate script.
All lines except those beginning with a ``>`` will be ignored, and the ``>`` will be stripped from the beginning of all the code lines.
There must be a blank line between each code section (lines beginning with ``>``) and comment section.
Grammars not using the literate notation must be in a file with the ``.y`` suffix.

.. _sec-lexical-rules:

Lexical Rules
-------------

Identifiers in Happy grammar files must take the following form (using
the BNF syntax from the Haskell Report):

::

   id      ::= alpha { idchar }
             | ' { any{^'} | \' } '
             | " { any{^"} | \" } "

   alpha   ::= A | B | ... | Z
             | a | b | ... | z

   idchar  ::= alpha
             | 0 | 1 | ... | 9
             | _

.. _sec-module-header:

Module Header
-------------

.. index::
  single: module; header

This section is optional, but if included takes the following form:

::

   {
   <Haskell module header>
   }

The Haskell module header contains the module name, exports, and imports.
No other code is allowed in the header—this is because Happy may need to include its own ``import`` statements directly after the user defined header.

.. _sec-directives:

Directives
----------

This section contains a number of lines of the form:

::

   %<directive name> <argument> ...

The statements here are all annotations to help Happy generate the Haskell code for the grammar.
Some of them are optional, and some of them are required.

.. _sec-token-type:

Token Type
~~~~~~~~~~

::

   %tokentype   { <valid Haskell type> }

.. index:: ``%tokentype``

(mandatory)
The ``%tokentype`` directive gives the type of the tokens passed from the lexical analyser to the parser
(in order that Happy can supply types for functions and data in the generated parser).

.. _sec-tokens:

Tokens
~~~~~~

::

   %token <name> { <Haskell pattern> }
          <name> { <Haskell pattern> }
          ...

.. index:: ``%token``

(mandatory)
The ``%token`` directive is used to tell Happy about all the terminal symbols used in the grammar.
Each terminal has a name, by which it is referred to in the grammar itself, and a Haskell representation enclosed in braces.
Each of the patterns must be of the same type, given by the ``%tokentype`` directive.

The name of each terminal follows the lexical rules for Happy identifiers given above.
There are no lexical differences between terminals and non-terminals in the grammar, so it is recommended that you stick to a convention;
for example using upper case letters for terminals and lower case for non-terminals, or vice-versa.

Happy will give you a warning if you try to use the same identifier both as a non-terminal and a terminal, or introduce an identifier which is declared as neither.

To save writing lots of projection functions that map tokens to their components, you can include ``$$`` in your Haskell pattern.
For example:

.. index:: ``$$``

::

   %token INT { TokenInt $$ }
          ...

This makes the semantic value of ``INT`` refer to the first argument of ``TokenInt`` rather than the whole token, eliminating the need for any projection function.

.. _sec-parser-name:

Parser Name
~~~~~~~~~~~

::

   %name <Haskell identifier> [ <non-terminal> ]
   ...

.. index:: ``%name``

(optional)
The ``%name`` directive is followed by a valid Haskell identifier, and gives the name of the top-level parsing function in the generated parser.
This is the only function that needs to be exported from a parser module.

If the ``%name`` directive is omitted, it defaults to ``happyParse``.

.. index:: ``happyParse``

The ``%name`` directive takes an optional second parameter which specifies the top-level non-terminal which is to be parsed.
If this parameter is omitted, it defaults to the first non-terminal defined in the grammar.

Multiple ``%name`` directives may be given, specifying multiple parser entry points for this grammar
(see :ref:`Generating Multiple Parsers From a Single Grammar <sec-multiple-parsers>`).
When multiple ``%name`` directives are given, they must all specify explicit non-terminals.

.. _sec-partial-parsers:

Partial Parsers
~~~~~~~~~~~~~~~

::

   %partial <Haskell identifier> [ <non-terminal> ]
   ...

.. index:: ``%partial``

The ``%partial`` directive can be used instead of ``%name``.
It indicates that the generated parser should be able to parse an initial portion of the input.
In contrast, a parser specified with ``%name`` will only parse the entire input.

A parser specified with ``%partial`` will stop parsing and return a result as soon as there exists a complete parse, and no more of the input can be parsed.
It does this by accepting the parse if it is followed by the ``error`` token, rather than insisting that the parse is followed by the end of the token stream
(or the ``eof`` token in the case of a ``%lexer`` parser).

.. _sec-monad-decl:

Monad Directive
~~~~~~~~~~~~~~~

::

   %monad { <type> } { <then> } { <return> }

.. index:: ``%monad``

(optional)
The ``%monad`` directive takes three arguments: the type constructor of the monad, the ``then`` (or ``bind``) operation, and the ``return`` (or ``unit``) operation.
The type constructor can be any type with kind ``* -> *``.

Monad declarations are described in more detail in :ref:`Monadic Parsers <sec-monads>`.

.. _sec-lexer-decl:

Lexical Analyser
~~~~~~~~~~~~~~~~

::

   %lexer { <lexer> } { <eof> }

.. index:: ``%lexer``

(optional)
The ``%lexer`` directive takes two arguments:
``<lexer>`` is the name of the lexical analyser function,
and ``<eof>`` is a token that is to be treated as the end of file.

Lexer declarations are described in more detail in :ref:`Threaded Lexers <sec-lexers>`.

.. _sec-prec-decls:

Precedence declarations
~~~~~~~~~~~~~~~~~~~~~~~

::

   %left     <name> ...
   %right    <name> ...
   %nonassoc <name> ...

.. index::
  single: ``%left`` directive
  single: ``%right`` directive
  single: ``%nonassoc`` directive

These declarations are used to specify the precedences and associativity of tokens.
The precedence assigned by a ``%left``, ``%right`` or ``%nonassoc`` declaration is defined to be higher than the precedence assigned by all declarations earlier in the file,
and lower than the precedence assigned by all declarations later in the file.

The associativity of a token relative to tokens in the same ``%left``, ``%right``, or ``%nonassoc`` declaration is to the left, to the right, or non-associative respectively.

Precedence declarations are described in more detail in :ref:`Using Precedences <sec-Precedences>`.

.. _sec-expect:

Expect declarations
~~~~~~~~~~~~~~~~~~~

::

   %expect <number>

.. index:: ``%expect`` directive

(optional)
More often than not the grammar you write will have conflicts.
These conflicts generate warnings.
But when you have checked the warnings and made sure that Happy handles them correctly these warnings are just annoying.
The ``%expect`` directive gives a way of avoiding them.
Declaring ``%expect n`` is a way of telling Happy
“There are exactly <n> shift/reduce conflicts and zero reduce/reduce conflicts in this grammar.
I promise I have checked them and they are resolved correctly”.
When processing the grammar, Happy will check the actual number of conflicts against the ``%expect`` declaration if any, and if there is a discrepancy then an error will be reported.

Happy's ``%expect`` directive works exactly like that of yacc.

.. _sec-error-directive:

Error declaration
~~~~~~~~~~~~~~~~~

::

   %error { <identifier> }

.. index:: ``%error``

Specifies the function to be called in the event of a parse error.
The type of ``<identifier>`` varies depending on the presence of ``%lexer`` (see :ref:`Summary <sec-monad-summary>`) and ``%errorhandlertype`` (see the following).

.. _sec-errorhandlertype-directive:

Additional error information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   %errorhandlertype (explist | default)

.. index:: ``%errorhandlertype``

(optional)
The expected type of the user-supplied error handling can be applied with additional information.
By default, no information is added, for compatibility with previous versions.
However, if ``explist`` is provided with this directive, then the first application will be of type ``[String]``,
providing a description of possible tokens that would not have failed the parser in place of the token that has caused the error.

.. _sec-attributes:

Attribute Type Declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   %attributetype { <valid Haskell type declaration> }

.. index:: ``%attributetype``

directive
(optional)
This directive allows you to declare the type of the attributes record when defining an attribute grammar.
If this declaration is not given, Happy will choose a default.
This declaration may only appear once in a grammar.

Attribute grammars are explained in :ref:`Attribute Grammars <sec-AttributeGrammar>`.

.. _sec-attribute:

Attribute declaration
~~~~~~~~~~~~~~~~~~~~~

::

   %attribute <Haskell identifier> { <valid Haskell type> }

.. index:: ``%attribute`` directive

The presence of one or more of these directives declares that the grammar is an attribute grammar.
The first attribute listed becomes the default attribute.
Each ``%attribute`` directive generates a field in the attributes record with the given label and type.
If there is an ``%attributetype`` declaration in the grammar which introduces type variables, then the type of an attribute may mention any such type variables.

Attribute grammars are explained in :ref:`Attribute Grammars <sec-AttributeGrammar>`.

.. _sec-grammar:

Grammar
-------

The grammar section comes after the directives, separated from them by a double-percent (``%%``) symbol.
This section contains a number of *productions*, each of which defines a single non-terminal.
Each production has the following syntax:

.. index:: ``%%``

::

   <non-terminal> [ :: { <type> } ]
           :  <id> ... {[%] <expression> }
         [ |  <id> ... {[%] <expression> }
           ... ]

The first line gives the non-terminal to be defined by the production and optionally its type
(type signatures for productions are discussed in :ref:`Type Signatures <sec-type-signatures>`).

Each production has at least one, and possibly many right-hand sides.
Each right-hand side consists of zero or more symbols (terminals or non-terminals) and a Haskell expression enclosed in braces.

The expression represents the semantic value of the non-terminal, and may refer to the semantic values of the symbols in the right-hand side using the meta-variables ``$1 ... $n``.
It is an error to refer to ``$i`` when ``i`` is larger than the number of symbols on the right hand side of the current rule.
The symbol ``$`` may be inserted literally in the Haskell expression using the sequence ``\$`` (this isn't necessary inside a string or character literal).

Additionally, the sequence ``$>`` can be used to represent the value of the rightmost symbol.

A semantic value of the form ``{% ... }`` is a *monadic action*, and is only valid when the grammar file contains a ``%monad`` directive (:ref:`Monad Directive <sec-monad-decl>`).
Monadic actions are discussed in :ref:`Monadic Parsers <sec-monads>`.

.. index::
  single: monadic; action

Remember that all the expressions for a production must have the same type.

.. _sec-param-prods:

Parameterized Productions
~~~~~~~~~~~~~~~~~~~~~~~~~

Starting from version 1.17.1, Happy supports *parameterized productions* which provide a convenient notation for capturing recurring patterns in context free grammars.
This gives the benefits of something similar to parsing combinators in the context of Happy grammars.

This functionality is best illustrated with an example:

::

   opt(p)          : p                   { Just $1 }
                   |                     { Nothing }

   rev_list1(p)    : p                   { [$1] }
                   | rev_list1(p) p      { $2 : $1 }

The first production, ``opt``, is used for optional components of a grammar.
It is just like ``p?`` in regular expressions or EBNF.
The second production, ``rev_list1``, is for parsing a list of 1 or more occurrences of ``p``.
Parameterized productions are just like ordinary productions, except that they have parameter in parenthesis after the production name.
Multiple parameters should be separated by commas:

::

   fst(p,q)        : p q                 { $1 }
   snd(p,q)        : p q                 { $2 }
   both(p,q)       : p q                 { ($1,$2) }

To use a parameterized production, we have to pass values for the parameters, as if we are calling a function.
The parameters can be either terminals, non-terminals, or other instantiations of parameterized productions.
Here are some examples:

::

   list1(p)        : rev_list1(p)        { reverse $1 }
   list(p)         : list1(p)            { $1 }
                   |                     { [] }

The first production uses ``rev_list`` to define a production that behaves like ``p+``, returning a list of elements in the same order as they occurred in the input.
The second one, ``list`` is like ``p*``.

Parameterized productions are implemented as a preprocessing pass in Happy:
each instantiation of a production turns into a separate non-terminal, but are careful to avoid generating the same rule multiple times, as this would lead to an ambiguous grammar.
Consider, for example, the following parameterized rule:

::

   sep1(p,q)       : p list(snd(q,p))    { $1 : $2 }

The rules that would be generated for ``sep1(EXPR,SEP)``

::

   sep1(EXPR,SEP)
     : EXPR list(snd(SEP,EXPR))                { $1 : $2 }

   list(snd(SEP,EXPR))
     : list1(snd(SEP,EXPR))                    { $1 }
     |                                         { [] }

   list1(snd(SEP,EXPR))
     : rev_list1(snd(SEP,EXPR))                { reverse $1 }

   rev_list1(snd(SEP,EXPR))
     : snd(SEP,EXPR))                          { [$1] }
     | rev_list1(snd(SEP,EXPR)) snd(SEP,EXPR)  { $2 : $1 }

   snd(SEP,EXPR)
     : SEP EXPR                                { $2 }

Note that this is just a normal grammar, with slightly strange names for the non-terminals.

A drawback of the current implementation is that it does not support type signatures for the parameterized productions, that depend on the types of the parameters.
We plan to implement that in the future --- the current workaround is to omit the type signatures for such rules.

.. _sec-module-trailer:

Module Trailer
--------------

.. index:: module; trailer

The module trailer is optional, comes right at the end of the grammar file, and takes the same form as the module header:

::

   {
   <Haskell code>
   }

This section is used for placing auxiliary definitions that need to be in the same module as the parser.
In small parsers, it often contains a hand-written lexical analyser too.
There is no restriction on what can be placed in the module trailer, and any code in there is copied verbatim into the generated parser file.
