#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
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

%monad { ParseM } { (QUALIFY(>>=)) } { QUALIFY(return) }

%token
        'S'             { TokenSucc }
        'Z'             { TokenZero }

%%

Exp         :       'Z'         { 0 }
            |       'S' Exp     { $2 QUALIFY(+) 1 }

{

type ParseM a = QUALIFY(Either) ParseError a
data ParseError
        = ParseError (QUALIFY(Maybe) Token)
        | StringError QUALIFY(String)
    deriving (QUALIFY(Eq), QUALIFY(Show))
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
    deriving (QUALIFY(Eq), QUALIFY(Show))

handleError :: [Token] -> ParseM a
handleError [] = throwError QUALIFY($) ParseError QUALIFY(Nothing)
handleError ts = throwError QUALIFY($) ParseError QUALIFY($) QUALIFY(Just) QUALIFY($) QUALIFY(head) ts

lexer :: QUALIFY(String) -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c QUALIFY(==) 'S'  = TokenSucc:(lexer cs)
    | c QUALIFY(==) 'Z'  = TokenZero:(lexer cs)
    | QUALIFY(otherwise) = QUALIFY(error) "lexer error"

main :: QUALIFY(IO) ()
main = do
    let tokens = lexer "S S"
    when (parseFoo tokens QUALIFY(/=) QUALIFY(Left) (ParseError QUALIFY(Nothing))) QUALIFY($) do
        QUALIFY(print) (parseFoo tokens)
        exitWith (ExitFailure 1)

---
class Error a where
    noMsg :: a
    noMsg = strMsg ""
    strMsg :: QUALIFY(String) -> a
class QUALIFY(Monad) m => MonadError e m | m -> e where
    throwError :: e -> m a
instance MonadError e (QUALIFY(Either) e) where
    throwError = QUALIFY(Left)
}
