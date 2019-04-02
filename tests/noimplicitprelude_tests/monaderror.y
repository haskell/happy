{
module Main where

import Data.Char
import Control.Monad.Error
import System.Exit

{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude
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
            |       'S' Exp     { $2 Prelude.+ 1 }

{

type ParseM a = Prelude.Either ParseError a
data ParseError
        = ParseError (Prelude.Maybe Token)
        | StringError Prelude.String
    deriving (Prelude.Eq,Prelude.Show)
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
    deriving (Prelude.Eq,Prelude.Show)

handleError :: [Token] -> ParseM a
handleError [] = throwError Prelude.$ ParseError Prelude.Nothing
handleError ts = throwError Prelude.$ ParseError Prelude.$ Prelude.Just Prelude.$ Prelude.head ts

lexer :: Prelude.String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c Prelude.== 'S'  = TokenSucc:(lexer cs)
    | c Prelude.== 'Z'  = TokenZero:(lexer cs)
    | Prelude.otherwise = Prelude.error "lexer error"

main :: Prelude.IO ()
main = do
    let tokens = lexer "S S"
    when (parseFoo tokens Prelude./= Prelude.Left (ParseError Prelude.Nothing)) Prelude.$ do
        Prelude.print (parseFoo tokens)
        exitWith (ExitFailure 1)
}
