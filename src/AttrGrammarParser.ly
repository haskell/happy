This parser parses the contents of the attribute grammar
into a list of rules.  A rule can either be an assignment
to an attribute of the LHS (synthesized attribute), and 
assignment to an attribute of the RHS (an inherited attribute),
or a conditional statement.

> {
> module AttrGrammarParser (agParser) where
> import ParseMonad
> import AttrGrammar
> }

> %name agParser
> %tokentype { AgToken }
> %token
>   "{"       { AgTok_LBrace }
>   "}"       { AgTok_RBrace }
>   ";"       { AgTok_Semicolon }
>   "="       { AgTok_Eq }
>   where     { AgTok_Where }
>   selfRef   { AgTok_SelfRef _ }
>   subRef    { AgTok_SubRef _ }
>   rightRef  { AgTok_RightmostRef _ }
>   unknown   { AgTok_Unknown _ }
>
> %monad { P }
> %lexer { agLexer } { AgTok_EOF }

> %%

> agParser :: { [AgRule] }
>   : rules                                      { $1 }

> rules :: { [AgRule] }
>   : rule ";" rules                             { $1 : $3 }
>   | rule                                       { $1 : [] }
>   |                                            { [] }

> rule :: { AgRule }
>   : selfRef  "=" code                          { SelfAssign (selfRefVal $1) $3 }
>   | subRef   "=" code                          { SubAssign (subRefVal $1) $3 }
>   | rightRef "=" code                          { RightmostAssign (rightRefVal $1) $3 }
>   | where code                                 { Conditional $2 }

> code :: { [AgToken] }
>   : "{" code0 "}" code                         { [$1] ++ $2 ++ [$3] ++ $4 }
>   | "=" code                                   { $1 : $2 }
>   | selfRef code                               { $1 : $2 }
>   | subRef code                                { $1 : $2 }
>   | rightRef code                              { $1 : $2 }
>   | unknown code                               { $1 : $2 }
>   |                                            { [] }

> code0 :: { [AgToken] }
>   : "{" code0 "}" code0                        { [$1] ++ $2 ++ [$3] ++ $4 }
>   | "=" code0                                  { $1 : $2 }
>   | ";" code0                                  { $1 : $2 }
>   | selfRef code0                              { $1 : $2 }
>   | subRef code0                               { $1 : $2 }
>   | rightRef code                              { $1 : $2 }
>   | unknown code0                              { $1 : $2 }
>   |                                            { [] }

> {
> happyError :: P a
> happyError = fail ("Parse error\n")
> }
