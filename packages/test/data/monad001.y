-- Testing %monad without %lexer, using the IO monad.

{
module Main where

import System.IO
import Data.Char
}

%name calc
%tokentype { Token }

%token 	num		{ TokenNum $$ }
	'+'		{ TokenPlus }
	'-'		{ TokenMinus }
	'*'		{ TokenTimes }
	'/'		{ TokenDiv }
	'^'		{ TokenExp }
	'\n'		{ TokenEOL }
	'('		{ TokenOB }
	')'		{ TokenCB }

%left '-' '+'
%left '*'
%nonassoc '/'
%left NEG     -- negation--unary minus
%right '^'    -- exponentiation

%monad { IO } { (>>=) } { return }

%%
input	: {- empty string -}   { () }
        | input line	      { $1 }

line	: '\n'		     { () }
        | exp '\n'  	     {% hPutStr stdout (show $1) }

exp	: num                { $1         }
        | exp '+' exp        { $1 + $3    }
        | exp '-' exp        { $1 - $3    }
        | exp '*' exp        { $1 * $3    }
        | exp '/' exp        { $1 / $3    }
        | '-' exp  %prec NEG { -$2        }
--        | exp '^' exp        { $1 ^ $3    }
        | '(' exp ')'        { $2         }

{
main = do
   calc (lexer "1 + 2 * 3 / 4\n")

    {-
   -- check that non-associative operators can't be used together
   r <- try (calc (lexer "1 / 2 / 3"))
   case r of 
       Left e  -> return ()
       Right _ -> ioError (userError "fail!")
-}

data Token
	= TokenExp
	| TokenEOL
	| TokenNum Double
	| TokenPlus
	| TokenMinus
	| TokenTimes
	| TokenDiv
	| TokenOB
	| TokenCB

-- and a simple lexer that returns this datastructure.

lexer :: String -> [Token]
lexer [] = []
lexer ('\n':cs) = TokenEOL : lexer cs
lexer (c:cs)
	| isSpace c = lexer cs
	| isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('^':cs) = TokenExp : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexNum cs = TokenNum (read num) : lexer rest
	where (num,rest) = span isNum cs
	      isNum c = isDigit c || c == '.'


happyError tokens = ioError (userError "parse error")
}
