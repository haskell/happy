{
-- only list imports here
import Char
}

%tokentype { Token }

%lexer { lexer } { TokenEOF }

%token
	'+' 	{ Sym '+' }
	i 	{ AnInt $$ }

%%

R : Q {}
Q : B Q i {} | S {} 
S : A S i {} | '+' {} | Q i {}
B : {}
A : {}


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
