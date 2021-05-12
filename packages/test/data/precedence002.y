-- This module demonstrates a bug in the original 1.11 release of Happy.

{
module Main where
import System.IO
import Control.Exception as Exception
}

%name parse

%tokentype { Tok }
%token
      '+'  { Plus }
      '/'  { Divide }
      int  { Num $$ }

%left '+'
%left '*'
%nonassoc '/'

%%
E    : E '+' E  { Plus' $1 $3 }
     | E '/' E  { Divide' $1 $3 }
     | int      { Num' $1 }

{
happyError :: [Tok] -> a
happyError s = error (concatMap show s)

data Tok = Plus | Divide | Num Int deriving Show

data Syn = Plus' Syn Syn | Divide' Syn Syn | Num' Int deriving Show

-- due to a bug in conflict resolution, this caused a parse error:
tokens1 = [Num 6, Divide, Num 7, Plus, Num 8]

main = print (parse tokens1)
}
