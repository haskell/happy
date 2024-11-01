{
module Main where

import Data.Char
}

%name parseExp Exp
%tokentype { Token }
%error { abort } { reportError }

%monad { ParseM } { (>>=) } { return }

%token
  '1' { TOne }
  '+' { TPlus }
  '(' { TOpen }
  ')' { TClose }

%right '+'
%expect 0     -- The point of this test: The List productions should expose a shift/reduce conflict because of catch

%%

Close :: { String }
Close : ')'   { ")" }
      | catch { "catch" }

Exp :: { String }
Exp : catch          { "catch" }
    | '1'            { "1"}
    | '(' List Close { "(" ++ $2 ++ $3 }

List :: { String }
     : Exp '+' { $1 ++ "+" }
     | Exp '+' Exp { $1 ++ "+" ++ $3 }

{
data Token = TOne | TPlus | TComma | TOpen | TClose

type ParseM = Maybe

abort :: [Token] -> ParseM a
abort = undefined

reportError :: [Token] -> ([Token] -> ParseM a) -> ParseM a
reportError = undefined

main :: IO ()
main = return ()
}
