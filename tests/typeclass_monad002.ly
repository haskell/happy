-----------------------------------------------------------------------------
Test for monadic Happy Parsers, Simon Marlow 1996.

#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

> {
> {-# OPTIONS_GHC -fglasgow-exts #-}
> -- -fglasgow-exts required because P is a type synonym, and Happy uses it
> -- unsaturated.
> import Data.Char
>
> }

> %name calc
> %tokentype { Token }

> %monad { (QUALIFY(Monad) m) } { P m } { thenP } { returnP }
> %lexer { lexer } { TokenEOF }

> %token
>	let		{ TokenLet }
>	in		{ TokenIn }
>	int		{ TokenInt $$ }
>	var		{ TokenVar $$ }
>	'='		{ TokenEq }
>	'+'		{ TokenPlus }
>	'-'		{ TokenMinus }
>	'*'		{ TokenTimes }
>	'/'		{ TokenDiv }
>	'('		{ TokenOB }
>	')'		{ TokenCB }

> %%

> Exp :: {Exp}
>     : let var '=' Exp in Exp	{% \s l -> QUALIFY(return) (ParseOk (Let l $2 $4 $6)) }
>     | Exp1			{ Exp1 $1 }
>
> Exp1 :: {Exp1}
>      : Exp1 '+' Term		{ Plus $1 $3 }
>      | Exp1 '-' Term		{ Minus $1 $3 }
>      | Term			{ Term $1 }
>
> Term :: {Term}
>      : Term '*' Factor	{ Times $1 $3 }
>      | Term '/' Factor	{ Div $1 $3 }
>      | Factor			{ Factor $1 }
>

> Factor :: {Factor}
>        : int			{ Int $1 }
> 	 | var			{ Var $1 }
> 	 | '(' Exp ')'		{ Brack $2 }

> {

-----------------------------------------------------------------------------
The monad serves three purposes:

	* it passes the input string around
	* it passes the current line number around
	* it deals with success/failure.

> data ParseResult a
>	= ParseOk a
>	| ParseFail QUALIFY(String)

> type P m a = QUALIFY(String) -> QUALIFY(Int) -> m (ParseResult a)

> thenP :: QUALIFY(Monad) m => P m a -> (a -> P m b) -> P m b
> m `thenP` k = \s l ->
>   do
>     res <- m s l
>     case res of
>       ParseFail s -> QUALIFY(return) (ParseFail s)
>       ParseOk a -> k a s l

> returnP :: QUALIFY(Monad) m => a -> P m a
> returnP a = \s l -> QUALIFY(return) (ParseOk a)

-----------------------------------------------------------------------------

Now we declare the datastructure that we are parsing.

> data Exp  = Let QUALIFY(Int) QUALIFY(String) Exp Exp | Exp1 Exp1
> data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term
> data Term = Times Term Factor | Div Term Factor | Factor Factor
> data Factor = Int QUALIFY(Int) | Var QUALIFY(String) | Brack Exp

The datastructure for the tokens...

> data Token
>	= TokenLet
>	| TokenIn
>	| TokenInt QUALIFY(Int)
>	| TokenVar QUALIFY(String)
>	| TokenEq
>	| TokenPlus
>	| TokenMinus
>	| TokenTimes
>	| TokenDiv
>	| TokenOB
>	| TokenCB
>	| TokenEOF

.. and a simple lexer that returns this datastructure.

> lexer :: QUALIFY(Monad) m => (Token -> P m a) -> P m a
> lexer cont s = case s of
> 	[] -> cont TokenEOF []
>  	('\n':cs) -> \line -> lexer cont cs (line QUALIFY(+) 1)
> 	(c:cs)
>               | isSpace c -> lexer cont cs
>               | isAlpha c -> lexVar (c:cs)
>               | isDigit c -> lexNum (c:cs)
> 	('=':cs) -> cont TokenEq cs
> 	('+':cs) -> cont TokenPlus cs
> 	('-':cs) -> cont TokenMinus cs
> 	('*':cs) -> cont TokenTimes cs
> 	('/':cs) -> cont TokenDiv cs
> 	('(':cs) -> cont TokenOB cs
> 	(')':cs) -> cont TokenCB cs
>  where
> 	lexNum cs = cont (TokenInt (QUALIFY(read) num)) rest
> 		where (num,rest) = QUALIFY(span) isDigit cs
> 	lexVar cs =
>    	    case QUALIFY(span) isAlpha cs of
> 		("let",rest) -> cont TokenLet rest
> 		("in",rest)  -> cont TokenIn rest
> 		(var,rest)   -> cont (TokenVar var) rest

> runCalc :: QUALIFY(Monad) m => QUALIFY(String) -> m Exp
> runCalc s =
>   do
>     res <- calc s 1
>     case res of
>       ParseOk e -> QUALIFY(return) e
>       ParseFail s -> QUALIFY(error) s

-----------------------------------------------------------------------------
The following functions should be defined for all parsers.

This is the overall type of the parser.

> type Parse m = P m Exp
> calc :: QUALIFY(Monad) m => Parse m

The next function is called when a parse error is detected.  It has
the same type as the top-level parse function.

> happyError :: P m a
> happyError = \s i -> QUALIFY(error) (
>	"Parse error in line " QUALIFY(++) QUALIFY(show) (i::QUALIFY(Int)) QUALIFY(++) "\n")

-----------------------------------------------------------------------------

Here we test our parser.

> main =
>   do
>     res <- runCalc "1 + 2 + 3"
>     case res of
>       (Exp1 (Plus (Plus (Term (Factor (Int 1)))
>             (Factor (Int 2))) (Factor (Int 3)))) ->
>         do
>           res <- runCalc "1 * 2 + 3"
>           case res of
>             (Exp1 (Plus (Term (Times (Factor (Int 1)) (Int 2)))
>                   (Factor (Int 3)))) ->
>               do
>                 res <- runCalc "1 + 2 * 3"
>                 case res of
>                   (Exp1 (Plus (Term (Factor (Int 1)))
>                         (Times (Factor (Int 2)) (Int 3)))) ->
>                     do
>                       res <- runCalc "let x = 2 in x * (x - 2)"
>                       case res of
>                         (Let 1 "x" (Exp1 (Term (Factor (Int 2))))
>                                    (Exp1 (Term (Times (Factor (Var "x"))
>                                    (Brack (Exp1 (Minus (Term (Factor (Var "x")))
>                                    (Factor (Int 2))))))))) ->
>                           QUALIFY(print) "Test works\n"
>                         _ -> quit
>                   _ -> quit
>             _ -> quit
>       _ -> quit
> quit = QUALIFY(print) "Test failed\n"
> }
