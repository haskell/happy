-----------------------------------------------------------------------------
Test for monadic Happy Parsers, Simon Marlow 1996.

> {
> import Char
> }

> %name calc
> %tokentype { Token }

> %monad { P } { thenP } { returnP }
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

> Exp   :: {Exp}
>       : let var '=' Exp in Exp	{% \s l -> ParseOk (Let l $2 $4 $6) }
>       | Exp1			{ Exp1 $1 }
> 
> Exp1 :: {Exp1}
>      : Exp1 '+' Term		{ Plus $1 $3 }
>      | Exp1 '-' Term		{ Minus $1 $3 }
>      | Term			{ Term $1 }
>      | error			{ Term (Factor (Int 1)) }
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
>	| ParseFail String

> type P a = String -> Int -> ParseResult a

> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s l -> 
>	case m s l of
>		ParseFail s -> ParseFail s
>		ParseOk a -> k a s l

> returnP :: a -> P a
> returnP a = \s l -> ParseOk a

-----------------------------------------------------------------------------

Now we declare the datastructure that we are parsing.

> data Exp  = Let Int String Exp Exp | Exp1 Exp1 
> data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term 
> data Term = Times Term Factor | Div Term Factor | Factor Factor 
> data Factor = Int Int | Var String | Brack Exp 

The datastructure for the tokens...

> data Token
>	= TokenLet
>	| TokenIn
>	| TokenInt Int
>	| TokenVar String
>	| TokenEq
>	| TokenPlus
>	| TokenMinus
>	| TokenTimes
>	| TokenDiv
>	| TokenOB
>	| TokenCB
>	| TokenEOF

.. and a simple lexer that returns this datastructure.

> lexer :: (Token -> P a) -> P a
> lexer cont s = case s of
> 	[] -> cont TokenEOF []
>  	('\n':cs) -> \line -> lexer cont cs (line+1)
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
> 	lexNum cs = cont (TokenInt (read num)) rest
> 		where (num,rest) = span isDigit cs
> 	lexVar cs =
>    	    case span isAlpha cs of
> 		("let",rest) -> cont TokenLet rest
> 		("in",rest)  -> cont TokenIn rest
> 		(var,rest)   -> cont (TokenVar var) rest

> runCalc :: String -> Exp
> runCalc s = case calc s 1 of
>		ParseOk e -> e
>		ParseFail s -> error s

-----------------------------------------------------------------------------
The following functions should be defined for all parsers.

This is the overall type of the parser.

> type Parse = P Exp
> calc :: Parse

The next function is called when a parse error is detected.  It has
the same type as the top-level parse function.

> happyError :: P a
> happyError = \s i -> error (
>	"Parse error in line " ++ show (i::Int) ++ "\n")

-----------------------------------------------------------------------------

Here we test our parser.

> main = case runCalc "1 + 2 + 3" of {
>	(Exp1 (Plus (Plus (Term (Factor (Int 1))) (Factor (Int 2))) (Factor (Int 3))))  ->
> 	case runCalc "1 * 2 + 3" of {
>	(Exp1 (Plus (Term (Times (Factor (Int 1)) (Int 2))) (Factor (Int 3)))) ->
>	case runCalc "1 + 2 * 3" of {
>	(Exp1 (Plus (Term (Factor (Int 1))) (Times (Factor (Int 2)) (Int 3)))) ->
>	case runCalc "+ 2 * 3" of {
>	(Exp1 (Plus (Term (Factor (Int 1))) (Times (Factor (Int 2)) (Int 3)))) ->
>	case runCalc "let x = 2 in x * (x - 2)" of {
>	(Let 1 "x" (Exp1 (Term (Factor (Int 2)))) (Exp1 (Term (Times (Factor (Var "x")) (Brack (Exp1 (Minus (Term (Factor (Var "x"))) (Factor (Int 2))))))))) -> print "Test works\n"; 
>	_ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit }
> quit = print "Test failed\n"
> }
