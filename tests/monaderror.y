#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{
module Main where

import Data.Char
import Control.Monad.Error
import System.Exit

}

%name parseFoo
%tokentype { Token }
%error { handleError }

%monad { ParseM } { (>>=) } { return }

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
}
