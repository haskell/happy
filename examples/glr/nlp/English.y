{
-- only list imports here
import Char
}

%tokentype { Token }

%lexer { lexer } { TokenEOF }

%token
	det 		{ Det $$ }
	prep 		{ Prep $$ }
	noun 		{ Noun $$ }
	transvb 	{ Verb Trans $$ }
	intransvb 	{ Verb Intrans $$ }

%%

S 
 : NP VP {}

NP
 : det noun {}
 | NP PP {}

PP 
 : prep NP {}

VP
 : transvb NP {}
 | intransvb  {}
 | VP PP      {}

{

data Token
	= TokenEOF
	| Noun String
	| Verb Arity String 
        | Prep String
        | Det String
  deriving (Show,Eq,Ord)

data Arity = Trans | Intrans deriving (Show,Eq,Ord)

lexer :: String -> [[Token]]
lexer = map lex_word . words

-- simple lexicon
-- (no claims to accuracy)

lex_word w@"the"       = [Det w]
lex_word w@"a"         = [Det w]
lex_word w@"some"      = [Det w]
lex_word w@"in"        = [Prep w]
lex_word w@"with"      = [Prep w]
lex_word w@"park"      = [Verb Trans w, Noun w]
lex_word w@"man"       = [Verb Trans w, Noun w]
lex_word w@"saw"       = [Verb Trans w, Verb Intrans w, Noun w]
lex_word w@"run"       = [Verb Trans w, Verb Intrans w, Noun w]
lex_word w@"race"      = [Verb Trans w, Verb Intrans w, Noun w]
lex_word w@"telescope" = [Verb Trans w, Verb Intrans w, Noun w]
lex_word w = error $ "Not know: " ++ show w
}
