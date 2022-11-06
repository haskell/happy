
.. _sec-info-files:

Info Files
==========

Happy info files, generated using the ``-i`` flag, are your most important tool for debugging errors in your grammar.
Although they can be quite verbose, the general concept behind them is quite simple.

An info file contains the following information:

#. A summary of all shift/reduce and reduce/reduce conflicts in the grammar.

#. Under section ``Grammar``, a summary of all the rules in the grammar.
   These rules correspond directly to your input file, absent the actual Haskell code that is to be run for each rules.
   A rule is written in the form ``<non-terminal> -> <id> ...``

#. Under section ``Terminals``, a summary of all the terminal tokens you may run against, as well as a the Haskell pattern which matches against them.
   This corresponds directly to the contents of your ``%token`` directive (:ref:`Tokens <sec-tokens>`).

#. Under section ``Non-terminals``, a summary of which rules apply to which productions.
   This is generally redundant with the ``Grammar`` section.

#. The primary section ``States``, which describes the state-machine Happy built for your grammar, and all of the transitions for each state.

#. Finally, some statistics ``Grammar Totals`` at the end of the file.

In general, you will be most interested in the ``States`` section, as it will give you information, in particular, about any conflicts your grammar may have.

.. _sec-info-files-states:

States
------

Although Happy does its best to insulate you from the vagaries of parser generation, it's important to know a little about how shift-reduce parsers work in order to be able to interpret the entries in the ``States`` section.

In general, a shift-reduce parser operates by maintaining parse stack, which tokens and productions are shifted onto or reduced off of.
The parser maintains a state machine, which accepts a token, performs some shift or reduce, and transitions to a new state for the next token.
Importantly, these states represent *multiple* possible productions,
because in general the parser does not know what the actual production for the tokens it's parsing is going to be.
There's no direct correspondence between the state-machine and the input grammar;
this is something you have to reverse engineer.

With this knowledge in mind, we can look at two example states from the example grammar from :ref:`Using <sec-using>`:

::

   State 5

           Exp1 -> Term .                                      (rule 5)
           Term -> Term . '*' Factor                           (rule 6)
           Term -> Term . '/' Factor                           (rule 7)

           in             reduce using rule 5
           '+'            reduce using rule 5
           '-'            reduce using rule 5
           '*'            shift, and enter state 11
           '/'            shift, and enter state 12
           ')'            reduce using rule 5
           %eof           reduce using rule 5

   State 9

           Factor -> '(' . Exp ')'                             (rule 11)

           let            shift, and enter state 2
           int            shift, and enter state 7
           var            shift, and enter state 8
           '('            shift, and enter state 9

           Exp            goto state 10
           Exp1           goto state 4
           Term           goto state 5
           Factor         goto state 6

For each state, the first set of lines describes the *rules* which correspond to this state.
A period ``.`` is inserted in the production to indicate where,
if this is indeed the correct production, we would have parsed up to.
In state 5, there are multiple rules, so we don't know if we are parsing an ``Exp1``, a multiplication or a division
(however, we do know there is a ``Term`` on the parse stack);
in state 9, there is only one rule, so we know we are definitely parsing a ``Factor``.

The next set of lines specifies the action and state transition that should occur given a token.
For example, if in state 5 we process the ``'*'`` token,
this token is shifted onto the parse stack and we transition to the state corresponding to the rule ``Term -> Term '*' . Factor``
(matching the token disambiguated which state we are in.)

Finally, for states which shift on non-terminals,
there will be a last set of lines saying what should be done after the non-terminal has been fully parsed;
this information is effectively the stack for the parser.
When a reduce occurs, these goto entries are used to determine what the next state should be.

.. _sec-info-files-conflicts:

Interpreting conflicts
----------------------

When you have a conflict, you will see an entry like this in your info file:

::

   State 432

           atype -> SIMPLEQUOTE '[' . comma_types0 ']'         (rule 318)
           sysdcon -> '[' . ']'                                (rule 613)

           '_'            shift, and enter state 60
           'as'           shift, and enter state 16

   ...

           ']'            shift, and enter state 381
                           (reduce using rule 328)

   ...

On large, complex grammars, determining what the conflict is can be a bit of an art,
since the state with the conflict may not have enough information to determine why a conflict is occurring).

In some cases, the rules associated with the state with the conflict will immediately give you enough guidance to determine what the ambiguous syntax is.
For example, in the miniature shift/reduce conflict described in :ref:`Conflict Tips <sec-conflict-tips>`,
the conflict looks like this:

::

   State 13

           exp -> exp . '+' exp0                               (rule 1)
           exp0 -> if exp then exp else exp .                  (rule 3)

           then           reduce using rule 3
           else           reduce using rule 3
           '+'            shift, and enter state 7
                           (reduce using rule 3)

           %eof           reduce using rule 3

Here, rule 3 makes it easy to imagine that we had been parsing a statement like ``if 1 then 2 else 3 + 4``;
the conflict arises from whether or not we should shift
(thus parsing as ``if 1 then 2 else (3 + 4)``)
or reduce
(thus parsing as ``(if 1 then 2 else 3) + 4``).

Sometimes, there's not as much helpful context in the error message; take this abridged example from GHC's parser:

::

   State 49

           type -> btype .                                     (rule 281)
           type -> btype . '->' ctype                          (rule 284)

           '->'           shift, and enter state 472
                           (reduce using rule 281)

A pair of rules like this doesn't always result in a shift/reduce conflict:
to reduce with rule 281 implies that, in some context when parsing the non-terminal ``type``,
it is possible for an ``'->'`` to occur immediately afterwards
(indeed these source rules are factored such that there is no rule of the form ``... -> type '->' ...``).

The best way this author knows how to sleuth this out is to look for instances of the token and check if any of the preceding non-terminals could terminate in a type:

::

           texp -> exp '->' texp                              (500)
           exp -> infixexp '::' sigtype                       (414)
           sigtype -> ctype                                   (260)
           ctype -> type                                      (274)

As it turns out, this shift/reduce conflict results from ambiguity for *view patterns*,
as in the code sample ``case v of { x :: T -> T ... }``.
