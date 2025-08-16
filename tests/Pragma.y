{-# OPTIONS_HAPPY --ghc --array --coerce --strict #-}

{
import Data.Char
}

%tokentype { Token }

%token
	'*' 	{ Sym '*' }
	'+' 	{ Sym '+' }
	'-' 	{ Sym '-' }
	'(' 	{ Sym '(' }
	')' 	{ Sym ')' }
	i 	{ AnInt $$ }

%%

E :: {Tree}
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

data Tree
	= Plus Tree Tree
	| Times Tree Tree
	| Minus Tree Tree
	| Pars Tree
	| Const Int
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs

lexer (c:cs) | c `elem` "+*-()"
 = Sym c : lexer cs

lexer (c:cs) | isDigit c
 = let (yes,no) = span isDigit cs in AnInt (read $ c:yes) : lexer no

happyError _ = error "Parse error"

main :: IO ()
main = print $ happyParse $ lexer "1+3"
}
