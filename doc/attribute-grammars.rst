
.. _sec-AttributeGrammar:

Attribute Grammars
==================

.. _sec-introAttributeGrammars:

Introduction
------------

Attribute grammars are a formalism for expressing syntax directed translation of a context-free grammar.
An introduction to attribute grammars may be found `here <http://www-rocq.inria.fr/oscar/www/fnc2/manual/node32.html>`__.
There is also an article in the Monad Reader about attribute grammars and a different approach to attribute grammars using Haskell `here <http://www.haskell.org/haskellwiki/The_Monad.Reader/Issue4/Why_Attribute_Grammars_Matter>`__.

The main practical difficulty that has prevented attribute grammars from gaining widespread use involves evaluating the attributes.
Attribute grammars generate non-trivial data dependency graphs that are difficult to evaluate using mainstream languages and techniques.
The solutions generally involve restricting the form of the grammars or using big hammers like topological sorts.
However, a language which supports lazy evaluation, such as Haskell, has no problem forming complex data dependency graphs and evaluating them.
The primary intellectual barrier to attribute grammar adoption seems to stem from the fact that most programmers have difficulty with the declarative nature of the specification.
Haskell programmers, on the other hand, have already embraced a purely functional language.
In short, the Haskell language and community seem like a perfect place to experiment with attribute grammars.

Embedding attribute grammars in Happy is easy because because Haskell supports three important features: higher order functions, labeled records, and lazy evaluation.
Attributes are encoded as fields in a labeled record.
The parse result of each non-terminal in the grammar is a function which takes a record of inherited attributes and returns a record of synthesized attributes.
In each production, the attributes of various non-terminals are bound together using ``let``.
Finally, at the end of the parse, a distinguished attribute is evaluated to be the final result.
Lazy evaluation takes care of evaluating each attribute in the correct order, resulting in an attribute grammar system that is capable of evaluating a fairly large class of attribute grammars.

Attribute grammars in Happy do not use any language extensions, so the parsers are Haskell 98 (assuming you don't use the GHC specific ``-g`` option).
Currently, attribute grammars cannot be generated for GLR parsers.
(It's not exactly clear how these features should interact...)

.. _sec-AtrributeGrammarsInHappy:

Attribute Grammars in Happy
---------------------------

.. _sec-declaringAttributes:

Declaring Attributes
~~~~~~~~~~~~~~~~~~~~

The presence of one or more ``%attribute`` directives indicates that a grammar is an attribute grammar.
Attributes are calculated properties that are associated with the non-terminals in a parse tree.
Each ``%attribute`` directive generates a field in the attributes record with the given name and type.

The first ``%attribute`` directive in a grammar defines the default attribute.
The default attribute is distinguished in two ways:
1) if no attribute specifier is given on an attribute reference, the default attribute is assumed (see `Semantic Rules <#sec-semanticRules>`__)
and
2) the value for the default attribute of the starting non-terminal becomes the return value of the parse.

Optionally, one may specify a type declaration for the attribute record using the ``%attributetype`` declaration.
This allows you to define the type given to the attribute record and, more importantly, allows you to introduce type variables that can be subsequently used in ``%attribute`` declarations.
If the ``%attributetype`` directive is given without any ``%attribute`` declarations, then the ``%attributetype`` declaration has no effect.

For example, the following declarations:

::

   %attributetype { MyAttributes a }
   %attribute value { a }
   %attribute num   { Int }
   %attribute label { String }

would generate this attribute record declaration in the parser:

::

   data MyAttributes a =
      HappyAttributes {
        value :: a,
        num :: Int,
        label :: String
      }

and ``value`` would be the default attribute.

.. _sec-semanticRules:

Semantic Rules
~~~~~~~~~~~~~~

In an ordinary Happy grammar, a production consists of a list of terminals and/or non-terminals followed by an uninterpreted code fragment enclosed in braces.
With an attribute grammar, the format is very similar, but the braces enclose a set of semantic rules rather than uninterpreted Haskell code.
Each semantic rule is either an attribute calculation or a conditional, and rules are separated by semicolons [3]_.

Both attribute calculations and conditionals may contain attribute references and/or terminal references.
Just like regular Happy grammars, the tokens ``$1`` through ``$<n>``, where ``n`` is the number of symbols in the production, refer to subtrees of the parse.
If the referenced symbol is a terminal, then the value of the reference is just the value of the terminal, the same way as in a regular Happy grammar.
If the referenced symbol is a non-terminal, then the reference may be followed by an attribute specifier, which is a dot followed by an attribute name.
If the attribute specifier is omitted, then the default attribute is assumed (the default attribute is the first attribute appearing in an ``%attribute`` declaration).
The special reference ``$$`` references the attributes of the current node in the parse tree; it behaves exactly like the numbered references.
Additionally, the reference ``$>`` always references the rightmost symbol in the production.

An attribute calculation rule is of the form:

::

   <attribute reference> = <Haskell expression>

A rule of this form defines the value of an attribute, possibly as a function of the attributes of ``$$`` (inherited attributes), the attributes of non-terminals in the production (synthesized attributes), or the values of terminals in the production.
The value for an attribute can only be defined once for a particular production.

The following rule calculates the default attribute of the current production in terms of the first and second items of the production (a synthesized attribute):

::

   $$ = $1 : $2

This rule calculates the length attribute of a non-terminal in terms of the length of the current non-terminal (an inherited attribute):

::

   $1.length = $$.length + 1

Conditional rules allow the rejection of strings due to context-sensitive properties.
All conditional rules have the form:

::

   where <Haskell expression>

For non-monadic parsers, all conditional expressions must be of the same (monomorphic) type.
At the end of the parse, the conditionals will be reduced using ``seq``, which gives the grammar an opportunity to call ``error`` with an informative message.
For monadic parsers, all conditional statements must have type ``Monad m => m ()`` where ``m`` is the monad in which the parser operates.
All conditionals will be sequenced at the end of the parse, which allows the conditionals to call ``fail`` with an informative message.

The following conditional rule will cause the (non-monadic) parser to fail if the inherited length attribute is not 0.

::

   where if $$.length == 0 then () else error "length not equal to 0"

This conditional is the monadic equivalent:

::

   where unless ($$.length == 0) (fail "length not equal to 0")

.. _sec-AttrGrammarLimits:

Limits of Happy Attribute Grammars
----------------------------------

If you are not careful, you can write an attribute grammar which fails to terminate.
This generally happens when semantic rules are written which cause a circular dependency on the value of an attribute.
Even if the value of the attribute is well-defined (that is, if a fixpoint calculation over attribute values will eventually converge to a unique solution), this attribute grammar system will not evaluate such grammars.

One practical way to overcome this limitation is to ensure that each attribute is always used in either a top-down (inherited) fashion or in a bottom-up (synthesized) fashion.
If the calculations are sufficiently lazy, one can "tie the knot" by synthesizing a value in one attribute, and then assigning that value to another, inherited attribute at some point in the parse tree.
This technique can be useful for common tasks like building symbol tables for a syntactic scope and making that table available to sub-nodes of the parse.

.. _sec-AttributeGrammarExample:

Example Attribute Grammars
--------------------------

The following two toy attribute grammars may prove instructive.
The first is an attribute grammar for the classic context-sensitive grammar { a^n b^n c^n \| n >= 0 }.
It demonstrates the use of conditionals, inherited and synthesized attributes.

::

   {
   module ABCParser (parse) where
   }

   %tokentype { Char }

   %token a { 'a' }
   %token b { 'b' }
   %token c { 'c' }
   %token newline { '\n' }

   %attributetype { Attrs a }
   %attribute value { a }
   %attribute len   { Int }

   %name parse abcstring

   %%

   abcstring
      : alist blist clist newline
           { $$ = $1 ++ $2 ++ $3
           ; $2.len = $1.len
           ; $3.len = $1.len
           }

   alist
      : a alist
           { $$ = $1 : $2
           ; $$.len = $2.len + 1
           }
      |    { $$ = []; $$.len = 0 }

   blist
      : b blist
           { $$ = $1 : $2
           ; $2.len = $$.len - 1
           }
      |    { $$ = []
           ; where failUnless ($$.len == 0) "blist wrong length"
           }

   clist
      : c clist
           { $$ = $1 : $2
           ; $2.len = $$.len - 1
           }
      |    { $$ = []
           ; where failUnless ($$.len == 0) "clist wrong length"
           }

   {
   happyError = error "parse error"
   failUnless b msg = if b then () else error msg
   }

This grammar parses binary numbers and calculates their value.
It demonstrates the use of inherited and synthesized attributes.

::

   {
   module BitsParser (parse) where
   }

   %tokentype { Char }

   %token minus { '-' }
   %token plus  { '+' }
   %token one   { '1' }
   %token zero  { '0' }
   %token newline { '\n' }

   %attributetype { Attrs }
   %attribute value { Integer }
   %attribute pos   { Int }

   %name parse start

   %%

   start
      : num newline { $$ = $1 }

   num
      : bits        { $$ = $1       ; $1.pos = 0 }
      | plus bits   { $$ = $2       ; $2.pos = 0 }
      | minus bits  { $$ = negate $2; $2.pos = 0 }

   bits
      : bit         { $$ = $1
                    ; $1.pos = $$.pos
                    }

      | bits bit    { $$ = $1 + $2
                    ; $1.pos = $$.pos + 1
                    ; $2.pos = $$.pos
                    }

   bit
      : zero        { $$ = 0 }
      | one         { $$ = 2^($$.pos) }

   {
   happyError = error "parse error"
   }

.. [3]
   Note that semantic rules must not rely on layout, because whitespace
   alignment is not guaranteed to be preserved
