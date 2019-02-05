{
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Main where

import System.IO
import Data.Char
}

%name calc
%tokentype { Token }

%token tok { Token }

%monad { IO } { (>>=) } { return }

%%

ib :: { (Int, Double, Bool) }
   : f n { ($1 $2, $1 $2, $1 True) }

f :: { forall a. a -> a }
  : { id }

n :: { forall a. Num a => a }
  : { 5 }

{
main = calc [] >>= print

data Token = Token

lexer :: String -> [Token]
lexer _ = []

happyError tokens = ioError (userError "parse error")
}
