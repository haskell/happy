This is a simple test for happy.

First thing to declare is the name of your parser,
and the type of the tokens the parser reads.

> {
> import Data.Char
>
> {-# LANGUAGE NoImplicitPrelude #-}
> import qualified Prelude
> }

> %name calc Exp
> %partial term Term
> %tokentype { Token }

The parser will be of type [Token] -> ?, where ? is determined by the
production rules.  Now we declare all the possible tokens:

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

The *new* system.

 %token
	let		( let )
	in		( in )
	int		( digit+ )
	var		( {alpha}{alphanum}+ )
	'='		( = )
	'+'		( + )
	'-'		( - )
	'*'		( * )
	'/'		( / )
	'('		( \( )
	')'		( \) )
 %whitespace		( {space}|{tab} )
 %newline		( {newline} )

The left hand side are the names of the terminals or tokens,
and the right hand side is how to pattern match them.

Like yacc, we include %% here, for no real reason.

> %%

Now we have the production rules.

> Exp :: { Exp }
> Exp : let var '=' Exp in Exp	{ Let $2 $4 $6 }
>     | Exp1			{ Exp1 $1 }
>
> Exp1 :: { Exp1 }
> Exp1 : Exp1 '+' Term		{ Plus $1 $3 }
>      | Exp1 '-' Term		{ Minus $1 $3 }
>      | Term			{ Term $1 }
>
> Term :: { Term }
> Term : Term '*' Factor	{ Times $1 $3 }
>      | Term '/' Factor	{ Div $1 $3 }
>      | Factor			{ Factor $1 }
>
> Factor :: { Factor }
> Factor : int			{ Int $1 }
> 	 | var			{ Var $1 }
> 	 | '(' Exp ')'		{ Brack $2 }

We are simply returning the parsed data structure !
Now we need some extra code, to support this parser,
and make in complete:

> {

All parsers must declair this function,
which is called when an error is detected.
Note that currently we do no error recovery.

> happyError tks = Prelude.error "Parse error"

Now we declare the datastructure that we are parsing.

> data Exp  = Let Prelude.String Exp Exp | Exp1 Exp1 deriving Prelude.Show
> data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term deriving Prelude.Show
> data Term = Times Term Factor | Div Term Factor | Factor Factor deriving Prelude.Show
> data Factor = Int Prelude.Int | Var Prelude.String | Brack Exp deriving Prelude.Show

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

.. and a simple lexer that returns this datastructure.

> lexer :: Prelude.String -> [Token]
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

> runTerm :: Prelude.String -> Term
> runTerm = term Prelude.. lexer

Here we test our parser.

> main = case runCalc "1 + 2 + 3" of {
>	(Exp1 (Plus (Plus (Term (Factor (Int 1))) (Factor (Int 2))) (Factor (Int 3))))  ->
> 	case runCalc "1 * 2 + 3" of {
>	(Exp1 (Plus (Term (Times (Factor (Int 1)) (Int 2))) (Factor (Int 3)))) ->
>	case runCalc "1 + 2 * 3" of {
>	(Exp1 (Plus (Term (Factor (Int 1))) (Times (Factor (Int 2)) (Int 3)))) ->
>	case runCalc "let x = 2 in x * (x - 2)" of {
>	(Let "x" (Exp1 (Term (Factor (Int 2)))) (Exp1 (Term (Times (Factor (Var "x")) (Brack (Exp1 (Minus (Term (Factor (Var "x"))) (Factor (Int 2))))))))) ->
>	case runTerm "1 + 2 * 3" of {
>	Factor (Int 1) ->
>	case runTerm "1*2+3" of {
>	Times (Factor (Int 1)) (Int 2) ->
>	case runTerm "1*2*3" of {
>	Times (Times (Factor (Int 1)) (Int 2)) (Int 3) ->
>       Prelude.print "Test works\n";
>	_ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit }
> quit = Prelude.print "Test failed\n"

> }
