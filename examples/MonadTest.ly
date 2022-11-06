-----------------------------------------------------------------------------
Tests %monad without %lexer.

> {
> import Data.Char
> }

> %name calc
> %tokentype { Token }

> %monad { P } { thenP } { returnP }

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
>     : let var '=' Exp in Exp	{ Let $2 $4 $6 }
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
The monad serves two purposes: 

	* it passes the current line number around
	* it deals with success/failure.

> data ParseResult a
>	= ParseOk a
>	| ParseFail String

> type P a = Int -> ParseResult a

> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \l -> 
>	case m l of
>		ParseFail s -> ParseFail s
>		ParseOk a -> k a l

> returnP :: a -> P a
> returnP a = \l -> ParseOk a

-----------------------------------------------------------------------------

Now we declare the datastructure that we are parsing.

> data Exp  = Let String Exp Exp | Exp1 Exp1 
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

> lexer :: String -> [Token]
> lexer [] = []
> lexer (c:cs) 
>	| isSpace c = lexer cs
> 	| isAlpha c = lexVar (c:cs)
>	| isDigit c = lexNum (c:cs)
> lexer ('=':cs) = TokenEq : lexer cs
> lexer ('+':cs) = TokenPlus : lexer cs
> lexer ('-':cs) = TokenMinus : lexer cs
> lexer ('*':cs) = TokenTimes : lexer cs
> lexer ('/':cs) = TokenDiv : lexer cs
> lexer ('(':cs) = TokenOB : lexer cs
> lexer (')':cs) = TokenCB : lexer cs

> lexNum cs = TokenInt (read num) : lexer rest
>	where (num,rest) = span isDigit cs

> lexVar cs =
>    case span isAlpha cs of
>	("let",rest) -> TokenLet : lexer rest
>	("in",rest)  -> TokenIn : lexer rest
>	(var,rest)   -> TokenVar var : lexer rest

> runCalc :: String -> Exp
> runCalc s = case calc (lexer s) 1 of
>		ParseOk e -> e
>		ParseFail s -> error s

> happyError = \tks i -> error (
>	"Parse error in line " ++ show (i::Int) ++ "\n")

-----------------------------------------------------------------------------

Here we test our parser.

> main = case runCalc "1 + 2 + 3" of {
>	(Exp1 (Plus (Plus (Term (Factor (Int 1))) (Factor (Int 2))) (Factor (Int 3))))  ->
> 	case runCalc "1 * 2 + 3" of {
>	(Exp1 (Plus (Term (Times (Factor (Int 1)) (Int 2))) (Factor (Int 3)))) ->
>	case runCalc "1 + 2 * 3" of {
>	(Exp1 (Plus (Term (Factor (Int 1))) (Times (Factor (Int 2)) (Int 3)))) ->
>	case runCalc "let x = 2 in x * (x - 2)" of {
>	(Let "x" (Exp1 (Term (Factor (Int 2)))) (Exp1 (Term (Times (Factor (Var "x")) (Brack (Exp1 (Minus (Term (Factor (Var "x"))) (Factor (Int 2))))))))) -> print "Test works\n"; 
>	_ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit }
> quit = print "Test failed\n"
> }
