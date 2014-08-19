{
-- only list imports here
import Data.Char
import Tree 
}

%tokentype { Token }

%lexer { lexer } { TokenEOF }

%token
	'*' 	{ Sym '*' }
	'+' 	{ Sym '+' }
	'-' 	{ Sym '-' }
	'(' 	{ Sym '(' }
	')' 	{ Sym ')' }
	i 	{ AnInt $$ }

%%

E :: {Tree ForestId Int}
 : E '+' E	{ Plus  $1 $3 }
 | E '*' E	{ Times $1 $3 }
 | E '-' E     	{ Minus $1 $3 }
 | '(' E ')'    { Pars  $2 } 
 | i            { Const $1 }



{

data Token
	= TokenEOF
	| Sym Char
	| AnInt {getInt :: Int}
  deriving (Show,Eq, Ord)


lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs

lexer (c:cs) | c `elem` "+*-()" 
 = Sym c : lexer cs

lexer (c:cs) | isDigit c 
 = let (yes,no) = span isDigit cs in AnInt (read $ c:yes) : lexer no

}
