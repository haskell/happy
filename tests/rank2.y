#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

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

%monad { QUALIFY(IO) } { (QUALIFY(>>=)) } { QUALIFY(return) }

%%

ib :: { (QUALIFY(Int), QUALIFY(Double), QUALIFY(Bool)) }
   : f n { ($1 $2, $1 $2, $1 QUALIFY(True)) }

f :: { forall a. a -> a }
  : { QUALIFY(id) }

n :: { forall a. QUALIFY(Num) a => a }
  : { 5 }

{
main = calc [] QUALIFY(>>=) print

data Token = Token

lexer :: QUALIFY(String) -> [Token]
lexer _ = []

happyError tokens = QUALIFY(ioError) (QUALIFY(userError) "parse error")
}
