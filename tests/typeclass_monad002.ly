-----------------------------------------------------------------------------
Test for monadic Happy Parsers, Simon Marlow 1996.

#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
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

> %monad { (QUALIFIEDPRELUDE.Monad m) } { P m } { thenP } { returnP }
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
>     : let var '=' Exp in Exp	{% \s l -> QUALIFIEDPRELUDE.return (ParseOk (Let l $2 $4 $6)) }
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
>	| ParseFail QUALIFIEDPRELUDE.String

> type P m a = QUALIFIEDPRELUDE.String -> QUALIFIEDPRELUDE.Int -> m (ParseResult a)

> thenP :: QUALIFIEDPRELUDE.Monad m => P m a -> (a -> P m b) -> P m b
> m `thenP` k = \s l ->
>   do
>     res <- m s l
>     case res of
>       ParseFail s -> QUALIFIEDPRELUDE.return (ParseFail s)
>       ParseOk a -> k a s l

> returnP :: QUALIFIEDPRELUDE.Monad m => a -> P m a
> returnP a = \s l -> QUALIFIEDPRELUDE.return (ParseOk a)

-----------------------------------------------------------------------------

Now we declare the datastructure that we are parsing.

> data Exp  = Let QUALIFIEDPRELUDE.Int QUALIFIEDPRELUDE.String Exp Exp | Exp1 Exp1
> data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term
> data Term = Times Term Factor | Div Term Factor | Factor Factor
> data Factor = Int QUALIFIEDPRELUDE.Int | Var QUALIFIEDPRELUDE.String | Brack Exp

The datastructure for the tokens...

> data Token
>	= TokenLet
>	| TokenIn
>	| TokenInt QUALIFIEDPRELUDE.Int
>	| TokenVar QUALIFIEDPRELUDE.String
>	| TokenEq
>	| TokenPlus
>	| TokenMinus
>	| TokenTimes
>	| TokenDiv
>	| TokenOB
>	| TokenCB
>	| TokenEOF

.. and a simple lexer that returns this datastructure.

> lexer :: QUALIFIEDPRELUDE.Monad m => (Token -> P m a) -> P m a
> lexer cont s = case s of
> 	[] -> cont TokenEOF []
>  	('\n':cs) -> \line -> lexer cont cs (line QUALIFIEDPRELUDE.+ 1)
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
> 	lexNum cs = cont (TokenInt (QUALIFIEDPRELUDE.read num)) rest
> 		where (num,rest) = QUALIFIEDPRELUDE.span isDigit cs
> 	lexVar cs =
>    	    case QUALIFIEDPRELUDE.span isAlpha cs of
> 		("let",rest) -> cont TokenLet rest
> 		("in",rest)  -> cont TokenIn rest
> 		(var,rest)   -> cont (TokenVar var) rest

> runCalc :: QUALIFIEDPRELUDE.Monad m => QUALIFIEDPRELUDE.String -> m Exp
> runCalc s =
>   do
>     res <- calc s 1
>     case res of
>       ParseOk e -> QUALIFIEDPRELUDE.return e
>       ParseFail s -> QUALIFIEDPRELUDE.error s

-----------------------------------------------------------------------------
The following functions should be defined for all parsers.

This is the overall type of the parser.

> type Parse m = P m Exp
> calc :: QUALIFIEDPRELUDE.Monad m => Parse m

The next function is called when a parse error is detected.  It has
the same type as the top-level parse function.

> happyError :: P m a
> happyError = \s i -> QUALIFIEDPRELUDE.error (
>	"Parse error in line " QUALIFIEDPRELUDE.++ QUALIFIEDPRELUDE.show (i::QUALIFIEDPRELUDE.Int) QUALIFIEDPRELUDE.++ "\n")

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
>                           QUALIFIEDPRELUDE.print "Test works\n"
>                         _ -> quit
>                   _ -> quit
>             _ -> quit
>       _ -> quit
> quit = QUALIFIEDPRELUDE.print "Test failed\n"
> }
