
.. _sec-using:

Using Happy
===========

Users of Yacc will find Happy quite familiar. The basic idea is as
follows:

-  Define the grammar you want to parse in a Happy grammar file.

-  Run the grammar through Happy, to generate a compilable Haskell
   module.

-  Use this module as part of your Haskell program, usually in
   conjunction with a lexical analyser (a function that splits the input
   into “tokens”, the basic unit of parsing).

Let's run through an example. We'll implement a parser for a simple
expression syntax, consisting of integers, variables, the operators
``+``, ``-``, ``*``, ``/``, and the form ``let var = exp in exp``. The
grammar file starts off like this:

::

   {
   module Main where
   }

At the top of the file is an optional module header, which is just a
Haskell module header enclosed in braces. This code is emitted verbatim
into the generated module, so you can put any Haskell code here at all.
In a grammar file, Haskell code is always contained between curly braces
to distinguish it from the grammar.

In this case, the parser will be a standalone program so we'll call the
module ``Main``.

Next comes a couple of declarations:

::

   %name calc
   %tokentype { Token }
   %error { parseError }

%name
%tokentype
%error
The first line declares the name of the parsing function that Happy will
generate, in this case ``calc``. In many cases, this is the only symbol
you need to export from the module.

The second line declares the type of tokens that the parser will accept.
The parser (i.e. the function ``calc``) will be of type
``[Token] -> T``, where ``T`` is the return type of the parser,
determined by the production rules below.

The ``%error`` directive tells Happy the name of a function it should
call in the event of a parse error. More about this later.

Now we declare all the possible tokens:

::

   %token
         let             { TokenLet }
         in              { TokenIn }
         int             { TokenInt $$ }
         var             { TokenVar $$ }
         '='             { TokenEq }
         '+'             { TokenPlus }
         '-'             { TokenMinus }
         '*'             { TokenTimes }
         '/'             { TokenDiv }
         '('             { TokenOB }
         ')'             { TokenCB }

%token
The symbols on the left are the tokens as they will be referred to in
the rest of the grammar, and to the right of each token enclosed in
braces is a Haskell pattern that matches the token. The parser will
expect to receive a stream of tokens, each of which will match one of
the given patterns (the definition of the ``Token`` datatype is given
later).

The ``$$`` symbol is a placeholder that represents the *value* of this
token. Normally the value of a token is the token itself, but by using
the ``$$`` symbol you can specify some component of the token object to
be the value.

$$
Like yacc, we include ``%%`` here, for no real reason.

::

   %%

Now we have the production rules for the grammar.

::

   Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
         | Exp1                    { Exp1 $1 }

   Exp1  : Exp1 '+' Term           { Plus $1 $3 }
         | Exp1 '-' Term           { Minus $1 $3 }
         | Term                    { Term $1 }

   Term  : Term '*' Factor         { Times $1 $3 }
         | Term '/' Factor         { Div $1 $3 }
         | Factor                  { Factor $1 }

   Factor
         : int                     { Int $1 }
         | var                     { Var $1 }
         | '(' Exp ')'             { Brack $2 }

non-terminal
Each production consists of a non-terminal symbol on the left, followed
by a colon, followed by one or more expansions on the right, separated
by ``|``. Each expansion has some Haskell code associated with it,
enclosed in braces as usual.

The way to think about a parser is with each symbol having a “value”: we
defined the values of the tokens above, and the grammar defines the
values of non-terminal symbols in terms of sequences of other symbols
(either tokens or non-terminals). In a production like this:

::

   n   : t_1 ... t_n   { E }

whenever the parser finds the symbols ``t_1...t_n`` in the token stream,
it constructs the symbol ``n`` and gives it the value ``E``, which may
refer to the values of ``t_1...t_n`` using the symbols ``$1...$n``.

The parser reduces the input using the rules in the grammar until just
one symbol remains: the first symbol defined in the grammar (namely
``Exp`` in our example). The value of this symbol is the return value
from the parser.

To complete the program, we need some extra code. The grammar file may
optionally contain a final code section, enclosed in curly braces.

::

   {

All parsers must include a function to be called in the event of a parse
error. In the ``%error`` directive earlier, we specified that the
function to be called on a parse error is ``parseError``:

::

   parseError :: [Token] -> a
   parseError _ = error "Parse error"

Note that ``parseError`` must be polymorphic in its return type ``a``,
which usually means it must be a call to ``error``. We'll see in
`Monadic Parsers <#sec-monads>`__ how to wrap the parser in a monad so
that we can do something more sensible with errors. It's also possible
to keep track of line numbers in the parser for use in error messages,
this is described in `Line Numbers <#sec-line-numbers>`__.

Next we can declare the data type that represents the parsed expression:

::

   data Exp
         = Let String Exp Exp
         | Exp1 Exp1
         deriving Show

   data Exp1
         = Plus Exp1 Term
         | Minus Exp1 Term
         | Term Term
         deriving Show

   data Term
         = Times Term Factor
         | Div Term Factor
         | Factor Factor
         deriving Show

   data Factor
         = Int Int
         | Var String
         | Brack Exp
         deriving Show

And the data structure for the tokens...

::

   data Token
         = TokenLet
         | TokenIn
         | TokenInt Int
         | TokenVar String
         | TokenEq
         | TokenPlus
         | TokenMinus
         | TokenTimes
         | TokenDiv
         | TokenOB
         | TokenCB
    deriving Show

... and a simple lexer that returns this data structure.

::

   lexer :: String -> [Token]
   lexer [] = []
   lexer (c:cs)
         | isSpace c = lexer cs
         | isAlpha c = lexVar (c:cs)
         | isDigit c = lexNum (c:cs)
   lexer ('=':cs) = TokenEq : lexer cs
   lexer ('+':cs) = TokenPlus : lexer cs
   lexer ('-':cs) = TokenMinus : lexer cs
   lexer ('*':cs) = TokenTimes : lexer cs
   lexer ('/':cs) = TokenDiv : lexer cs
   lexer ('(':cs) = TokenOB : lexer cs
   lexer (')':cs) = TokenCB : lexer cs

   lexNum cs = TokenInt (read num) : lexer rest
         where (num,rest) = span isDigit cs

   lexVar cs =
      case span isAlpha cs of
         ("let",rest) -> TokenLet : lexer rest
         ("in",rest)  -> TokenIn : lexer rest
         (var,rest)   -> TokenVar var : lexer rest

And finally a top-level function to take some input, parse it, and print
out the result.

::

   main = getContents >>= print . calc . lexer
   }

And that's it! A whole lexer, parser and grammar in a few dozen lines.
Another good example is Happy's own parser. Several features in Happy
were developed using this as an example.

info file
To generate the Haskell module for this parser, type the command
``happy example.y`` (where ``example.y`` is the name of the grammar
file). The Haskell module will be placed in a file named ``example.hs``.
Additionally, invoking the command ``happy example.y -i`` will produce
the file ``example.info`` which contains detailed information about the
parser, including states and reduction rules (see `Info
Files <#sec-info-files>`__). This can be invaluable for debugging
parsers, but requires some knowledge of the operation of a shift-reduce
parser.

.. _sec-other-datatypes:

Returning other datatypes
-------------------------

In the above example, we used a data type to represent the syntax being
parsed. However, there's no reason why it has to be this way: you could
calculate the value of the expression on the fly, using productions like
this:

::

   Term  : Term '*' Factor         { $1 * $3 }
         | Term '/' Factor         { $1 / $3 }
         | Factor                  { $1 }

The value of a ``Term`` would be the value of the expression itself, and
the parser could return an integer.

This works for simple expression types, but our grammar includes
variables and the ``let`` syntax. How do we know the value of a variable
while we're parsing it? We don't, but since the Haskell code for a
production can be anything at all, we could make it a function that
takes an environment of variable values, and returns the computed value
of the expression:

::

   Exp   : let var '=' Exp in Exp  { \p -> $6 (($2,$4 p):p) }
         | Exp1                    { $1 }

   Exp1  : Exp1 '+' Term           { \p -> $1 p + $3 p }
         | Exp1 '-' Term           { \p -> $1 p - $3 p }
         | Term                    { $1 }

   Term  : Term '*' Factor         { \p -> $1 p * $3 p }
         | Term '/' Factor         { \p -> $1 p `div` $3 p }
         | Factor                  { $1 }

   Factor
         : int                     { \p -> $1 }
         | var                     { \p -> case lookup $1 p of
                                           Nothing -> error "no var"
                           Just i  -> i }
         | '(' Exp ')'             { $2 }

The value of each production is a function from an environment *p* to a
value. When parsing a ``let`` construct, we extend the environment with
the new binding to find the value of the body, and the rule for ``var``
looks up its value in the environment. There's something you can't do in
``yacc`` :-)

.. _sec-sequences:

Parsing sequences
-----------------

A common feature in grammars is a *sequence* of a particular syntactic
element. In EBNF, we'd write something like ``n+`` to represent a
sequence of one or more ``n``\ s, and ``n*`` for zero or more. Happy
doesn't support this syntax explicitly, but you can define the
equivalent sequences using simple productions.

For example, the grammar for Happy itself contains a rule like this:

::

   prods : prod                   { [$1] }
         | prods prod             { $2 : $1 }

In other words, a sequence of productions is either a single production,
or a sequence of productions followed by a single production. This
recursive rule defines a sequence of one or more productions.

One thing to note about this rule is that we used *left recursion* to
define it - we could have written it like this:

recursion, left vs. right
::

   prods : prod                  { [$1] }
         | prod prods            { $1 : $2 }

The only reason we used left recursion is that Happy is more efficient
at parsing left-recursive rules; they result in a constant stack-space
parser, whereas right-recursive rules require stack space proportional
to the length of the list being parsed. This can be extremely important
where long sequences are involved, for instance in automatically
generated output. For example, the parser in GHC used to use
right-recursion to parse lists, and as a result it failed to parse some
Happy-generated modules due to running out of stack space!

One implication of using left recursion is that the resulting list comes
out reversed, and you have to reverse it again to get it in the original
order. Take a look at the Happy grammar for Haskell for many examples of
this.

Parsing sequences of zero or more elements requires a trivial change to
the above pattern:

::

   prods : {- empty -}           { [] }
         | prods prod            { $2 : $1 }

Yes - empty productions are allowed. The normal convention is to include
the comment ``{- empty -}`` to make it more obvious to a reader of the
code what's going on.

.. _sec-separators:

Sequences with separators
~~~~~~~~~~~~~~~~~~~~~~~~~

A common type of sequence is one with a *separator*: for instance
function bodies in C consist of statements separated by semicolons. To
parse this kind of sequence we use a production like this:

::

   stmts : stmt                   { [$1] }
         | stmts ';' stmt         { $3 : $1 }

If the ``;`` is to be a *terminator* rather than a separator (i.e. there
should be one following each statement), we can remove the semicolon
from the above rule and redefine ``stmt`` as

::

   stmt : stmt1 ';'              { $1 }

where ``stmt1`` is the real definition of statements.

We might like to allow extra semicolons between statements, to be a bit
more liberal in what we allow as legal syntax. We probably just want the
parser to ignore these extra semicolons, and not generate a \``null
statement'' value or something. The following rule parses a sequence of
zero or more statements separated by semicolons, in which the statements
may be empty:

::

   stmts : stmts ';' stmt          { $3 : $1 }
         | stmts ';'               { $1 }
         | stmt            { [$1] }
         | {- empty -}     { [] }

Parsing sequences of *one* or more possibly null statements is left as
an exercise for the reader...

.. _sec-Precedences:

Using Precedences
-----------------

precedences
associativity
Going back to our earlier expression-parsing example, wouldn't it be
nicer if we didn't have to explicitly separate the expressions into
terms and factors, merely to make it clear that ``'*'`` and ``'/'``
operators bind more tightly than ``'+'`` and ``'-'``?

We could just change the grammar as follows (making the appropriate
changes to the expression datatype too):

::

   Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
         | Exp '+' Exp             { Plus $1 $3 }
         | Exp '-' Exp             { Minus $1 $3 }
         | Exp '*' Exp             { Times $1 $3 }
         | Exp '/' Exp             { Div $1 $3 }
         | '(' Exp ')'             { Brack $2 }
         | int                     { Int $1 }
         | var                     { Var $1 }

but now Happy will complain that there are shift/reduce conflicts
because the grammar is ambiguous - we haven't specified whether e.g.
``1 + 2 * 3`` is to be parsed as ``1 + (2 * 3)`` or ``(1 + 2) * 3``.
Happy allows these ambiguities to be resolved by specifying the
precedences of the operators involved using directives in the
header [2]_:

::

   ...
   %right in
   %left '+' '-'
   %left '*' '/'
   %%
   ...

%left
directive
%right
directive
%nonassoc
directive
The ``%left`` or ``%right`` directive is followed by a list of
terminals, and declares all these tokens to be left or right-associative
respectively. The precedence of these tokens with respect to other
tokens is established by the order of the ``%left`` and ``%right``
directives: earlier means lower precedence. A higher precedence causes
an operator to bind more tightly; in our example above, because ``'*'``
has a higher precedence than ``'+'``, the expression ``1 + 2 * 3`` will
parse as ``1 + (2 * 3)``.

What happens when two operators have the same precedence? This is when
the associativity comes into play. Operators specified as left
associative will cause expressions like ``1 + 2 - 3`` to parse as
``(1 + 2) - 3``, whereas right-associative operators would parse as
``1 + (2 - 3)``. There is also a ``%nonassoc`` directive which indicates
that the specified operators may not be used together. For example, if
we add the comparison operators ``'>'`` and ``'<'`` to our grammar, then
we would probably give their precedence as:

::

   ...
   %right in
   %nonassoc '>' '<'
   %left '+' '-'
   %left '*' '/'
   %%
   ...

which indicates that ``'>'`` and ``'<'`` bind less tightly than the
other operators, and the non-associativity causes expressions such as
``1 > 2 > 3`` to be disallowed.

How precedence works
~~~~~~~~~~~~~~~~~~~~

The precedence directives, ``%left``, ``%right`` and ``%nonassoc``,
assign precedence levels to the tokens in the declaration. A rule in the
grammar may also have a precedence: if the last terminal in the right
hand side of the rule has a precedence, then this is the precedence of
the whole rule.

The precedences are used to resolve ambiguities in the grammar. If there
is a shift/reduce conflict, then the precedence of the rule and the
lookahead token are examined in order to resolve the conflict:

-  If the precedence of the rule is higher, then the conflict is
   resolved as a reduce.

-  If the precedence of the lookahead token is higher, then the conflict
   is resolved as a shift.

-  If the precedences are equal, then

   -  If the token is left-associative, then reduce

   -  If the token is right-associative, then shift

   -  If the token is non-associative, then fail

-  If either the rule or the token has no precedence, then the default
   is to shift (these conflicts are reported by Happy, whereas ones that
   are automatically resolved by the precedence rules are not).

.. _context-precedence:

Context-dependent Precedence
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The precedence of an individual rule can be overriden, using context
precedence. This is useful when, for example, a particular token has a
different precedence depending on the context. A common example is the
minus sign: it has high precedence when used as prefix negation, but a
lower precedence when used as binary subtraction.

We can implement this in Happy as follows:

::

   %right in
   %nonassoc '>' '<'
   %left '+' '-'
   %left '*' '/'
   %left NEG
   %%

   Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
         | Exp '+' Exp             { Plus $1 $3 }
         | Exp '-' Exp             { Minus $1 $3 }
         | Exp '*' Exp             { Times $1 $3 }
         | Exp '/' Exp             { Div $1 $3 }
         | '(' Exp ')'             { Brack $2 }
         | '-' Exp %prec NEG       { Negate $2 }
         | int                     { Int $1 }
         | var                     { Var $1 }

%prec
directive
We invent a new token ``NEG`` as a placeholder for the precedence of our
prefix negation rule. The ``NEG`` token doesn't need to appear in a
``%token`` directive. The prefix negation rule has a ``%prec NEG``
directive attached, which overrides the default precedence for the rule
(which would normally be the precedence of '-') with the precedence of
``NEG``.

.. _shift-directive:

The %shift directive for lowest precedence rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rules annotated with the ``%shift`` directive have the lowest possible
precedence and are non-associative. A shift/reduce conflict that
involves such a rule is resolved as a shift. One can think of ``%shift``
as ``%prec SHIFT`` such that ``SHIFT`` has lower precedence than any
other token.

This is useful in conjunction with ``%expect 0`` to explicitly point out
all rules in the grammar that result in conflicts, and thereby resolve
such conflicts.

.. _sec-type-signatures:

Type Signatures
---------------

type
signatures in grammar
Happy allows you to include type signatures in the grammar file itself,
to indicate the type of each production. This has several benefits:

-  Documentation: including types in the grammar helps to document the
   grammar for someone else (and indeed yourself) reading the code.

-  Fixing type errors in the generated module can become slightly easier
   if Happy has inserted type signatures for you. This is a slightly
   dubious benefit, since type errors in the generated module are still
   somewhat difficult to find.

-  Type signatures generally help the Haskell compiler to compile the
   parser faster. This is important when really large grammar files are
   being used.

The syntax for type signatures in the grammar file is as follows:

::

   stmts   :: { [ Stmt ] }
   stmts   : stmts stmt                { $2 : $1 }
       | stmt                      { [$1] }

In fact, you can leave out the superfluous occurrence of ``stmts``:

::

   stmts   :: { [ Stmt ] }
       : stmts stmt                { $2 : $1 }
       | stmt                      { [$1] }

Note that currently, you have to include type signatures for *all* the
productions in the grammar to benefit from the second and third points
above. This is due to boring technical reasons, but it is hoped that
this restriction can be removed in the future.

It is possible to have productions with polymorphic or overloaded types.
However, because the type of each production becomes the argument type
of a constructor in an algebraic datatype in the generated source file,
compiling the generated file requires a compiler that supports local
universal quantification. GHC (with the ``-fglasgow-exts`` option) and
Hugs are known to support this.

.. _sec-monads:

Monadic Parsers
---------------

monadic
parsers
Happy has support for threading a monad through the generated parser.
This might be useful for several reasons:

-  Handling parse errors by using an exception monad (see `Handling
   Parse Errors <#sec-exception>`__).

-  Keeping track of line numbers in the input file, for example for use
   in error messages (see `Line Numbers <#sec-line-numbers>`__).

-  Performing IO operations during parsing.

-  Parsing languages with context-dependencies (such as C) require some
   state in the parser.

Adding monadic support to your parser couldn't be simpler. Just add the
following directive to the declaration section of the grammar file:

::

   %monad { <type> } [ { <then> } { <return> } ]

%monad
where ``<type>`` is the type constructor for the monad, ``<then>`` is
the bind operation of the monad, and ``<return>`` is the return
operation. If you leave out the names for the bind and return
operations, Happy assumes that ``<type>`` is an instance of the standard
Haskell type class ``Monad`` and uses the overloaded names for the bind
and return operations.

When this declaration is included in the grammar, Happy makes a couple
of changes to the generated parser: the types of the main parser
function and ``parseError`` (the function named in ``%error``) become
``[Token] -> P a`` where ``P`` is the monad type constructor, and the
function must be polymorphic in ``a``. In other words, Happy adds an
application of the ``<return>`` operation defined in the declaration
above, around the result of the parser (``parseError`` is affected
because it must have the same return type as the parser). And that's all
it does.

This still isn't very useful: all you can do is return something of
monadic type from ``parseError``. How do you specify that the
productions can also have type ``P a``? Most of the time, you don't want
a production to have this type: you'd have to write explicit
``returnP``\ s everywhere. However, there may be a few rules in a
grammar that need to get at the monad, so Happy has a special syntax for
monadic actions:

::

   n  :  t_1 ... t_n          {% <expr> }

monadic
actions
The ``%`` in the action indicates that this is a monadic action, with
type ``P a``, where ``a`` is the real return type of the production.
When Happy reduces one of these rules, it evaluates the expression

::

   <expr> `then` \result -> <continue parsing>

Happy uses ``result`` as the real semantic value of the production.
During parsing, several monadic actions might be reduced, resulting in a
sequence like

::

   <expr1> `then` \r1 ->
   <expr2> `then` \r2 ->
   ...
   return <expr3>

The monadic actions are performed in the order that they are *reduced*.
If we consider the parse as a tree, then reductions happen in a
depth-first left-to-right manner. The great thing about adding a monad
to your parser is that it doesn't impose any performance overhead for
normal reductions - only the monadic ones are translated like this.

Take a look at the Haskell parser for a good illustration of how to use
a monad in your parser: it contains examples of all the principles
discussed in this section, namely parse errors, a threaded lexer,
line/column numbers, and state communication between the parser and
lexer.

The following sections consider a couple of uses for monadic parsers,
and describe how to also thread the monad through the lexical analyser.

.. _sec-exception:

Handling Parse Errors
~~~~~~~~~~~~~~~~~~~~~

parse errors
handling
It's not very convenient to just call ``error`` when a parse error is
detected: in a robust setting, you'd like the program to recover
gracefully and report a useful error message to the user. Exceptions (of
which errors are a special case) are normally implemented in Haskell by
using an exception monad, something like:

::

   data E a = Ok a | Failed String

   thenE :: E a -> (a -> E b) -> E b
   m `thenE` k =
      case m of
          Ok a     -> k a
          Failed e -> Failed e

   returnE :: a -> E a
   returnE a = Ok a

   failE :: String -> E a
   failE err = Failed err

   catchE :: E a -> (String -> E a) -> E a
   catchE m k =
      case m of
         Ok a     -> Ok a
         Failed e -> k e

This monad just uses a string as the error type. The functions ``thenE``
and ``returnE`` are the usual bind and return operations of the monad,
``failE`` raises an error, and ``catchE`` is a combinator for handling
exceptions.

We can add this monad to the parser with the declaration

::

   %monad { E } { thenE } { returnE }

Now, without changing the grammar, we can change the definition of
``parseError`` and have something sensible happen for a parse error:

::

   parseError tokens = failE "Parse error"

The parser now raises an exception in the monad instead of bombing out
on a parse error.

We can also generate errors during parsing. There are times when it is
more convenient to parse a more general language than that which is
actually intended, and check it later. An example comes from Haskell,
where the precedence values in infix declarations must be between 0 and
9:

::

   prec :: { Int }
         : int    {% if $1 < 0 || $1 > 9
                       then failE "Precedence out of range"
                   else returnE $1
           }

The monadic action allows the check to be placed in the parser itself,
where it belongs.

.. _sec-lexers:

Threaded Lexers
~~~~~~~~~~~~~~~

lexer, threaded
monadic
lexer
Happy allows the monad concept to be extended to the lexical analyser,
too. This has several useful consequences:

-  Lexical errors can be treated in the same way as parse errors, using
   an exception monad.

   parse errors
   lexical

-  Information such as the current file and line number can be
   communicated between the lexer and parser.

-  General state communication between the parser and lexer - for
   example, implementation of the Haskell layout rule requires this kind
   of interaction.

-  IO operations can be performed in the lexer - this could be useful
   for following import/include declarations for instance.

A monadic lexer is requested by adding the following declaration to the
grammar file:

::

   %lexer { <lexer> } { <eof> }

%lexer
where ``<lexer>`` is the name of the lexical analyser function, and
``<eof>`` is a token that is to be treated as the end of file.

When using a monadic lexer, the parser no longer reads a list of tokens.
Instead, it calls the lexical analysis function for each new token to be
read. This has the side effect of eliminating the intermediate list of
tokens, which is a slight performance win.

The type of the main parser function is now just ``P a`` - the input is
being handled completely within the monad.

The type of ``parseError`` becomes ``Token -> P a``; that is it takes
Happy's current lookahead token as input. This can be useful, because
the error function probably wants to report the token at which the parse
error occurred, and otherwise the lexer would have to store this token
in the monad.

The lexical analysis function must have the following type:

::

   lexer :: (Token -> P a) -> P a

where ``P`` is the monad type constructor declared with ``%monad``, and
``a`` can be replaced by the parser return type if desired.

You can see from this type that the lexer takes a *continuation* as an
argument. The lexer is to find the next token, and pass it to this
continuation to carry on with the parse. Obviously, we need to keep
track of the input in the monad somehow, so that the lexer can do
something different each time it's called!

Let's take the exception monad above, and extend it to add the input
string so that we can use it with a threaded lexer.

::

   data ParseResult a = Ok a | Failed String
   type P a = String -> ParseResult a

   thenP :: P a -> (a -> P b) -> P b
   m `thenP` k = \s ->
      case m s of
          Ok a     -> k a s
          Failed e -> Failed e

   returnP :: a -> P a
   returnP a = \s -> Ok a

   failP :: String -> P a
   failP err = \s -> Failed err

   catchP :: P a -> (String -> P a) -> P a
   catchP m k = \s ->
      case m s of
         Ok a     -> Ok a
         Failed e -> k e s

Notice that this isn't a real state monad - the input string just gets
passed around, not returned. Our lexer will now look something like
this:

::

   lexer :: (Token -> P a) -> P a
   lexer cont s =
       ... lexical analysis code ...
       cont token s'

the lexer grabs the continuation and the input string, finds the next
token ``token``, and passes it together with the remaining input string
``s'`` to the continuation.

We can now indicate lexical errors by ignoring the continuation and
calling ``failP "error message" s`` within the lexer (don't forget to
pass the input string to make the types work out).

This may all seem a bit weird. Why, you ask, doesn't the lexer just have
type ``P Token``? It was done this way for performance reasons - this
formulation sometimes means that you can use a reader monad instead of a
state monad for ``P``, and the reader monad might be faster. It's not at
all clear that this reasoning still holds (or indeed ever held), and
it's entirely possible that the use of a continuation here is just a
misfeature.

If you want a lexer of type ``P Token``, then just define a wrapper to
deal with the continuation:

::

   lexwrap :: (Token -> P a) -> P a
   lexwrap cont = real_lexer `thenP` \token -> cont token

Monadic productions with %lexer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``{% ... }`` actions work fine with ``%lexer``, but additionally
there are two more forms which are useful in certain cases. Firstly:

::

   n  :  t_1 ... t_n          {%^ <expr> }

In this case, ``<expr>`` has type ``Token -> P a``. That is, Happy
passes the current lookahead token to the monadic action ``<expr>``.
This is a useful way to get hold of Happy's current lookahead token
without having to store it in the monad.

::

   n  :  t_1 ... t_n          {%% <expr> }

This is a slight variant on the previous form. The type of ``<expr>`` is
the same, but in this case the lookahead token is actually discarded and
a new token is read from the input. This can be useful when you want to
change the next token and continue parsing.

.. _sec-line-numbers:

Line Numbers
~~~~~~~~~~~~

line numbers
%newline
Previous versions of Happy had a ``%newline`` directive that enabled
simple line numbers to be counted by the parser and referenced in the
actions. We warned you that this facility may go away and be replaced by
something more general, well guess what? :-)

Line numbers can now be dealt with quite straightforwardly using a
monadic parser/lexer combination. Ok, we have to extend the monad a bit
more:

::

   type LineNumber = Int
   type P a = String -> LineNumber -> ParseResult a

   getLineNo :: P LineNumber
   getLineNo = \s l -> Ok l

(the rest of the functions in the monad follow by just adding the extra
line number argument in the same way as the input string). Again, the
line number is just passed down, not returned: this is OK because of the
continuation-based lexer that can change the line number and pass the
new one to the continuation.

The lexer can now update the line number as follows:

::

   lexer cont s =
     case s of
        '\n':s  ->  \line -> lexer cont s (line + 1)
        ... rest of lexical analysis ...

It's as simple as that. Take a look at Happy's own parser if you have
the sources lying around, it uses a monad just like the one above.

Reporting the line number of a parse error is achieved by changing
``parseError`` to look something like this:

::

   parseError :: Token -> P a
   parseError = getLineNo `thenP` \line ->
                failP (show line ++ ": parse error")

We can also get hold of the line number during parsing, to put it in the
parsed data structure for future reference. A good way to do this is to
have a production in the grammar that returns the current line number:

::

   lineno :: { LineNumber }
           : {- empty -}      {% getLineNo }

The semantic value of ``lineno`` is the line number of the last token
read - this will always be the token directly following the ``lineno``
symbol in the grammar, since Happy always keeps one lookahead token in
reserve.

.. _sec-monad-summary:

Summary
~~~~~~~

The types of various functions related to the parser are dependent on
what combination of ``%monad`` and ``%lexer`` directives are present in
the grammar. For reference, we list those types here. In the following
types, *t* is the return type of the parser. A type containing a type
variable indicates that the specified function must be polymorphic.

type
of
parseError
type
of parser
type
of lexer

-

   .. container:: formalpara-title

      **No ``%monad`` or ``%lexer``**

   ::

      parse      :: [Token] -> t
      parseError :: [Token] -> a

-

   .. container:: formalpara-title

      **with ``%monad``**

   ::

      parse      :: [Token] -> P t
      parseError :: [Token] -> P a

-

   .. container:: formalpara-title

      **with ``%lexer``**

   ::

      parse      :: T t
      parseError :: Token -> T a
      lexer      :: (Token -> T a) -> T a

   where the type constructor ``T`` is whatever you want (usually
   ``T a = String -> a``). I'm not sure if this is useful, or even if it
   works properly.

-

   .. container:: formalpara-title

      **with ``%monad`` and ``%lexer``**

   ::

      parse      :: P t
      parseError :: Token -> P a
      lexer      :: (Token -> P a) -> P a

.. _sec-error:

The Error Token
---------------

error token
Happy supports a limited form of error recovery, using the special
symbol ``error`` in a grammar file. When Happy finds a parse error
during parsing, it automatically inserts the ``error`` symbol; if your
grammar deals with ``error`` explicitly, then it can detect the error
and carry on.

For example, the Happy grammar for Haskell uses error recovery to
implement Haskell layout. The grammar has a rule that looks like this:

::

   close : '}'                  { () }
         | error            { () }

This says that a close brace in a layout-indented context may be either
a curly brace (inserted by the lexical analyser), or a parse error.

This rule is used to parse expressions like ``let x = e in e'``: the
layout system inserts an open brace before ``x``, and the occurrence of
the ``in`` symbol generates a parse error, which is interpreted as a
close brace by the above rule.

yacc
Note for ``yacc`` users: this form of error recovery is strictly more
limited than that provided by ``yacc``. During a parse error condition,
``yacc`` attempts to discard states and tokens in order to get back into
a state where parsing may continue; Happy doesn't do this. The reason is
that normal ``yacc`` error recovery is notoriously hard to describe, and
the semantics depend heavily on the workings of a shift-reduce parser.
Furthermore, different implementations of ``yacc`` appear to implement
error recovery differently. Happy's limited error recovery on the other
hand is well-defined, as is just sufficient to implement the Haskell
layout rule (which is why it was added in the first place).

.. _sec-multiple-parsers:

Generating Multiple Parsers From a Single Grammar
-------------------------------------------------

multiple parsers
It is often useful to use a single grammar to describe multiple parsers,
where each parser has a different top-level non-terminal, but parts of
the grammar are shared between parsers. A classic example of this is an
interpreter, which needs to be able to parse both entire files and
single expressions: the expression grammar is likely to be identical for
the two parsers, so we would like to use a single grammar but have two
entry points.

Happy lets you do this by allowing multiple ``%name`` directives in the
grammar file. The ``%name`` directive takes an optional second parameter
specifying the top-level non-terminal for this parser, so we may specify
multiple parsers like so:

%name
directive
::

   %name parse1 non-terminal1
   %name parse2 non-terminal2

Happy will generate from this a module which defines two functions
``parse1`` and ``parse2``, which parse the grammars given by
``non-terminal1`` and ``non-terminal2`` respectively. Each parsing
function will of course have a different type, depending on the type of
the appropriate non-terminal.

.. [2]
   Users of ``yacc`` will find this familiar, Happy's precedence scheme
   works in exactly the same way.
