#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{
{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
module Main where

import Control.Monad (when)
import Data.Char
import System.Exit

}

%name parseFoo
%tokentype { Token }
%error { handleError }

%monad { ParseM } { (QUALIFIEDPRELUDE.>>=) } { QUALIFIEDPRELUDE.return }

%token
        'S'             { TokenSucc }
        'Z'             { TokenZero }

%%

Exp         :       'Z'         { 0 }
            |       'S' Exp     { $2 QUALIFIEDPRELUDE.+ 1 }

{

type ParseM a = QUALIFIEDPRELUDE.Either ParseError a
data ParseError
        = ParseError (QUALIFIEDPRELUDE.Maybe Token)
        | StringError QUALIFIEDPRELUDE.String
    deriving (QUALIFIEDPRELUDE.Eq,QUALIFIEDPRELUDE.Show)
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
    deriving (QUALIFIEDPRELUDE.Eq,QUALIFIEDPRELUDE.Show)

handleError :: [Token] -> ParseM a
handleError [] = throwError QUALIFIEDPRELUDE.$ ParseError QUALIFIEDPRELUDE.Nothing
handleError ts = throwError QUALIFIEDPRELUDE.$ ParseError QUALIFIEDPRELUDE.$ QUALIFIEDPRELUDE.Just QUALIFIEDPRELUDE.$ QUALIFIEDPRELUDE.head ts

lexer :: QUALIFIEDPRELUDE.String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c QUALIFIEDPRELUDE.== 'S'  = TokenSucc:(lexer cs)
    | c QUALIFIEDPRELUDE.== 'Z'  = TokenZero:(lexer cs)
    | QUALIFIEDPRELUDE.otherwise = QUALIFIEDPRELUDE.error "lexer error"

main :: QUALIFIEDPRELUDE.IO ()
main = do
    let tokens = lexer "S S"
    when (parseFoo tokens QUALIFIEDPRELUDE./= QUALIFIEDPRELUDE.Left (ParseError QUALIFIEDPRELUDE.Nothing)) QUALIFIEDPRELUDE.$ do
        QUALIFIEDPRELUDE.print (parseFoo tokens)
        exitWith (ExitFailure 1)

---
class Error a where
    noMsg :: a
    noMsg = strMsg ""
    strMsg :: QUALIFIEDPRELUDE.String -> a
class QUALIFIEDPRELUDE.Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
instance MonadError e (QUALIFIEDPRELUDE.Either e) where
    throwError = QUALIFIEDPRELUDE.Left
}
