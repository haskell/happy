#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
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

%monad { QUALIFIEDPRELUDE.IO } { (QUALIFIEDPRELUDE.>>=) } { QUALIFIEDPRELUDE.return }

%%

ib :: { (QUALIFIEDPRELUDE.Int, QUALIFIEDPRELUDE.Double, QUALIFIEDPRELUDE.Bool) }
   : f n { ($1 $2, $1 $2, $1 QUALIFIEDPRELUDE.True) }

f :: { forall a. a -> a }
  : { QUALIFIEDPRELUDE.id }

n :: { forall a. QUALIFIEDPRELUDE.Num a => a }
  : { 5 }

{
main = calc [] QUALIFIEDPRELUDE.>>= print

data Token = Token

lexer :: QUALIFIEDPRELUDE.String -> [Token]
lexer _ = []

happyError tokens = QUALIFIEDPRELUDE.ioError (QUALIFIEDPRELUDE.userError "parse error")
}
