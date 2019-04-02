This is a simple test for happy using operator precedence.

First thing to declare is the name of your parser,
and the type of the tokens the parser reads.

> {
> import Data.Char
>
> {-# LANGUAGE NoImplicitPrelude #-}
> import qualified Prelude
> }

> %name calc
> %tokentype { Token }

The parser will be of type [Token] -> ?, where ? is determined by the
production rules.  Now we declare all the possible tokens:

> %token
>	let		{ TokenLet }
>	in		{ TokenIn }
>	int		{ TokenInt $$ }
>	var		{ TokenVar $$ }
>	'='		{ TokenEq }
>	'>'		{ TokenGreater }
>	'<'		{ TokenLess }
>	'+'		{ TokenPlus }
>	'-'		{ TokenMinus }
>	'*'		{ TokenTimes }
>	'/'		{ TokenDiv }
>	'('		{ TokenOB }
>	')'		{ TokenCB }
>	UMINUS		{ TokenFoo }

> %nonassoc '>' '<'
> %left '+' '-'
> %left '*' '/'
> %left	UMINUS

> %%

> Exp :: { Exp }
> Exp : let var '=' Exp in Exp	{ Let $2 $4 $6 }
>     | Exp '>' Exp		{ Greater $1 $3 }
>     | Exp '<' Exp		{ Less $1 $3 }
>     | Exp '+' Exp		{ Plus $1 $3 }
>     | Exp '-' Exp		{ Minus $1 $3 }
>     | Exp '*' Exp		{ Times $1 $3 }
>     | Exp '/' Exp		{ Div $1 $3 }
>     | '-' Exp %prec UMINUS	{ Uminus $2 }
>     | '(' Exp ')'		{ Brack $2 }
>     | int			{ Int $1 }
>     | var			{ Var $1 }

We are simply returning the parsed data structure !
Now we need some extra code, to support this parser,
and make in complete:

> {

All parsers must declair this function,
which is called when an error is detected.
Note that currently we do no error recovery.

> happyError tks = Prelude.error "Parse error"

Now we declare the datastructure that we are parsing.

> data Exp
>	= Let Prelude.String Exp Exp
>	| Greater Exp Exp
>	| Less Exp Exp
>	| Plus Exp Exp
>	| Minus Exp Exp
> 	| Times Exp Exp
>	| Div Exp Exp
>	| Uminus Exp
>	| Brack Exp
>	| Int Prelude.Int
>	| Var Prelude.String
>	deriving Prelude.Show

The datastructure for the tokens...

> data Token
>	= TokenLet
>	| TokenIn
>	| TokenInt Prelude.Int
>	| TokenVar Prelude.String
>	| TokenEq
>	| TokenGreater
>	| TokenLess
>	| TokenPlus
>	| TokenMinus
>	| TokenTimes
>	| TokenDiv
>	| TokenOB
>	| TokenCB
>	| TokenFoo

.. and a simple lexer that returns this datastructure.

> lexer :: Prelude.String -> [Token]
> lexer [] = []
> lexer (c:cs)
>	| isSpace c = lexer cs
> 	| isAlpha c = lexVar (c:cs)
>	| isDigit c = lexNum (c:cs)
> lexer ('=':cs) = TokenEq : lexer cs
> lexer ('>':cs) = TokenGreater : lexer cs
> lexer ('<':cs) = TokenLess : lexer cs
> lexer ('+':cs) = TokenPlus : lexer cs
> lexer ('-':cs) = TokenMinus : lexer cs
> lexer ('*':cs) = TokenTimes : lexer cs
> lexer ('/':cs) = TokenDiv : lexer cs
> lexer ('(':cs) = TokenOB : lexer cs
> lexer (')':cs) = TokenCB : lexer cs

> lexNum cs = TokenInt (Prelude.read num) : lexer rest
>	where (num,rest) = Prelude.span isDigit cs

> lexVar cs =
>    case Prelude.span isAlpha cs of
>	("let",rest) -> TokenLet : lexer rest
>	("in",rest)  -> TokenIn : lexer rest
>	(var,rest)   -> TokenVar var : lexer rest

To run the program, call this in gofer, or use some code
to print it.

> runCalc :: Prelude.String -> Exp
> runCalc = calc Prelude.. lexer

Here we test our parser.

> main = case runCalc "let x = 1 in let y = 2 in x * y + x / y" of {
>       (Let "x" (Int 1) (Let "y" (Int 2) (Plus (Times (Var "x") (Var "y")) (Div (Var "x") (Var "y"))))) ->
>       case runCalc "- 1 * - 2 + 3" of {
>       (Plus (Times (Uminus (Int 1)) (Uminus (Int 2))) (Int 3)) ->
>       case runCalc "- - - 1 + 2 * 3 - 4" of {
>       (Minus (Plus (Uminus (Uminus (Uminus (Int 1)))) (Times (Int 2) (Int 3))) (Int 4)) ->
>       Prelude.print "Test works\n";
>       _ -> quit } ; _ -> quit } ; _ -> quit }
>
> quit = Prelude.print "Test failed\n";
>
> }
