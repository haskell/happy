{
-- only list imports here
import Data.Char
}

%tokentype { Token }

%lexer { lexer } { TokenEOF }

%token
	'b' 	{ Sym _ }

%%
-- grammar taken from
--	"Generalised LR Parsing in Haskell"
--	Joao Fernandes, Joao Saraiva, and Joost Visser
--	Universidade do Minho, Braga, Portugal 
--	submitted to AFP'04 summer school
--	(Original source of grammar not identified by them)

S : T {}
T : A 'b' {} | T T T {}
A : T 'b' A A A {} | T T 'b' {} | {}

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
