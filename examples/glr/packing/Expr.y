{
-- only list imports here
import Char
}

%tokentype { Token }

%lexer { lexer } { TokenEOF }

%token
	i 	{ Thing }

%%

S : A S {} | {}

A : B {}
B : C {}
C : D {} | E {}
D : i {}
E : i F {} 
F : {} 


{

data Token
	= TokenEOF
	| Thing
  deriving (Show,Eq, Ord)


lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs

lexer (c:cs) = Thing : lexer cs


}
