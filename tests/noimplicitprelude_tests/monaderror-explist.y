{
module Main where

import Data.Char
import Control.Monad.Error
import System.Exit
import System.Environment (getProgName)
import Data.List (isPrefixOf)

{-# LANGUAGE NoImplicitPrelude #-}
import qualified Prelude
}

%name parseFoo
%tokentype { Token }
%errorhandlertype explist
%error { handleErrorExpList }

%monad { ParseM } { (>>=) } { return }

%token
        'S'             { TokenSucc }
        'Z'             { TokenZero }
        'T'             { TokenTest }

%%

Exp         :       'Z'           { 0 }
            |       'T' 'Z' Exp   { $3 Prelude.+ 1 }
            |       'S' Exp       { $2 Prelude.+ 1 }

{

type ParseM a = Prelude.Either ParseError a
data ParseError
        = ParseError (Prelude.Maybe (Token, [Prelude.String]))
        | StringError Prelude.String
    deriving (Prelude.Eq,Prelude.Show)
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
	| TokenTest
    deriving (Prelude.Eq,Prelude.Show)

handleErrorExpList :: ([Token], [Prelude.String]) -> ParseM a
handleErrorExpList ([], _) = throwError Prelude.$ ParseError Prelude.Nothing
handleErrorExpList (ts, explist) = throwError Prelude.$ ParseError Prelude.$ Prelude.Just Prelude.$ (Prelude.head ts, explist)

lexer :: Prelude.String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c Prelude.== 'S'  = TokenSucc:(lexer cs)
    | c Prelude.== 'Z'  = TokenZero:(lexer cs)
    | c Prelude.== 'T'  = TokenTest:(lexer cs)
    | Prelude.otherwise = Prelude.error "lexer error"

main :: Prelude.IO ()
main = do
  test "Z Z" Prelude.$ Prelude.Left (ParseError (Prelude.Just (TokenZero,[])))
  test "T S" Prelude.$ Prelude.Left (ParseError (Prelude.Just (TokenSucc,["'Z'"])))

  where
    test inp exp = do
      Prelude.putStrLn Prelude.$ "testing " Prelude.++ inp
      let tokens = lexer inp
      when (parseFoo tokens Prelude./= exp) Prelude.$ do
        Prelude.print (parseFoo tokens)
        exitWith (ExitFailure 1)
}
