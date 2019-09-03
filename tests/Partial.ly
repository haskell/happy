This is a simple test for happy.

First thing to declare is the name of your parser,
and the type of the tokens the parser reads.

#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

> {
> import Data.Char
>
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

> happyError tks = QUALIFIEDPRELUDE.error "Parse error"

Now we declare the datastructure that we are parsing.

> data Exp  = Let QUALIFIEDPRELUDE.String Exp Exp | Exp1 Exp1 deriving QUALIFIEDPRELUDE.Show
> data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term deriving QUALIFIEDPRELUDE.Show
> data Term = Times Term Factor | Div Term Factor | Factor Factor deriving QUALIFIEDPRELUDE.Show
> data Factor = Int QUALIFIEDPRELUDE.Int | Var QUALIFIEDPRELUDE.String | Brack Exp deriving QUALIFIEDPRELUDE.Show

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

.. and a simple lexer that returns this datastructure.

> lexer :: QUALIFIEDPRELUDE.String -> [Token]
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

> lexNum cs = TokenInt (QUALIFIEDPRELUDE.read num) : lexer rest
>	where (num,rest) = QUALIFIEDPRELUDE.span isDigit cs

> lexVar cs =
>    case QUALIFIEDPRELUDE.span isAlpha cs of
>	("let",rest) -> TokenLet : lexer rest
>	("in",rest)  -> TokenIn : lexer rest
>	(var,rest)   -> TokenVar var : lexer rest

To run the program, call this in gofer, or use some code
to print it.

> runCalc :: QUALIFIEDPRELUDE.String -> Exp
> runCalc = calc QUALIFIEDPRELUDE.. lexer

> runTerm :: QUALIFIEDPRELUDE.String -> Term
> runTerm = term QUALIFIEDPRELUDE.. lexer

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
>       QUALIFIEDPRELUDE.print "Test works\n";
>	_ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit } ; _ -> quit }
> quit = QUALIFIEDPRELUDE.print "Test failed\n"

> }
