{
-- only list imports here
import Char
}

%tokentype { Token }

%monad { IO } { (>>=) } { return }
%lexer { lexer } { TokenEOF }

%token
	'*' 	{ Sym '*' }
	'+' 	{ Sym '+' }
	'-' 	{ Sym '-' }
	'(' 	{ Sym '(' }
	')' 	{ Sym ')' }
	i 	{ AnInt $$ }

%%

E :: {Int}
 : E '+' E	{% {-print ($1,$3) >>-} if odd $3 then fail "odd num" else return ($1 + $3) }
 | E '*' E	{ $1 * $3 }
 | E '-' E     	{ $1 - $3 }
 | '(' E ')'    { $2 } 
 | i            { $1 }




{

data Token
	= TokenEOF
	| Sym Char
	| AnInt Int
  deriving (Show,Eq, Ord)


lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs

lexer (c:cs) | c `elem` "+*-()" 
 = Sym c : lexer cs

lexer (c:cs) | isDigit c 
 = let (yes,no) = span isDigit cs in AnInt (read $ c:yes) : lexer no

}
