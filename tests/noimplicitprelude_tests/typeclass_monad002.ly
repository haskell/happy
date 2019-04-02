-----------------------------------------------------------------------------
Test for monadic Happy Parsers, Simon Marlow 1996.

> {
> {-# OPTIONS_GHC -fglasgow-exts #-}
> -- -fglasgow-exts required because P is a type synonym, and Happy uses it
> -- unsaturated.
> import Data.Char
>
> {-# LANGUAGE NoImplicitPrelude #-}
> import qualified Prelude
>
> }

> %name calc
> %tokentype { Token }

> %monad { (Prelude.Monad m) } { P m } { thenP } { returnP }
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
>     : let var '=' Exp in Exp	{% \s l -> Prelude.return (ParseOk (Let l $2 $4 $6)) }
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
>	| ParseFail Prelude.String

> type P m a = Prelude.String -> Prelude.Int -> m (ParseResult a)

> thenP :: Prelude.Monad m => P m a -> (a -> P m b) -> P m b
> m `thenP` k = \s l ->
>   do
>     res <- m s l
>     case res of
>       ParseFail s -> Prelude.return (ParseFail s)
>       ParseOk a -> k a s l

> returnP :: Prelude.Monad m => a -> P m a
> returnP a = \s l -> Prelude.return (ParseOk a)

-----------------------------------------------------------------------------

Now we declare the datastructure that we are parsing.

> data Exp  = Let Prelude.Int Prelude.String Exp Exp | Exp1 Exp1
> data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term
> data Term = Times Term Factor | Div Term Factor | Factor Factor
> data Factor = Int Prelude.Int | Var Prelude.String | Brack Exp

The datastructure for the tokens...

> data Token
>	= TokenLet
>	| TokenIn
>	| TokenInt Prelude.Int
>	| TokenVar Prelude.String
>	| TokenEq
>	| TokenPlus
>	| TokenMinus
>	| TokenTimes
>	| TokenDiv
>	| TokenOB
>	| TokenCB
>	| TokenEOF

.. and a simple lexer that returns this datastructure.

> lexer :: Prelude.Monad m => (Token -> P m a) -> P m a
> lexer cont s = case s of
> 	[] -> cont TokenEOF []
>  	('\n':cs) -> \line -> lexer cont cs (line Prelude.+ 1)
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
> 	lexNum cs = cont (TokenInt (Prelude.read num)) rest
> 		where (num,rest) = Prelude.span isDigit cs
> 	lexVar cs =
>    	    case Prelude.span isAlpha cs of
> 		("let",rest) -> cont TokenLet rest
> 		("in",rest)  -> cont TokenIn rest
> 		(var,rest)   -> cont (TokenVar var) rest

> runCalc :: Prelude.Monad m => Prelude.String -> m Exp
> runCalc s =
>   do
>     res <- calc s 1
>     case res of
>       ParseOk e -> Prelude.return e
>       ParseFail s -> Prelude.error s

-----------------------------------------------------------------------------
The following functions should be defined for all parsers.

This is the overall type of the parser.

> type Parse m = P m Exp
> calc :: Prelude.Monad m => Parse m

The next function is called when a parse error is detected.  It has
the same type as the top-level parse function.

> happyError :: P m a
> happyError = \s i -> Prelude.error (
>	"Parse error in line " Prelude.++ Prelude.show (i::Prelude.Int) Prelude.++ "\n")

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
>                           Prelude.print "Test works\n"
>                         _ -> quit
>                   _ -> quit
>             _ -> quit
>       _ -> quit
> quit = Prelude.print "Test failed\n"
> }
