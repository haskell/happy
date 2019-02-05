-- Testing %monad without %lexer, using the IO monad.

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

ib :: { (Int, Bool) }
   : f { ($1 5, $1 True) }

f :: { forall a. a -> a }
  : { id }

{
main = return ()

data Token = Token

lexer :: String -> [Token]
lexer _ = []

happyError tokens = ioError (userError "parse error")
}
