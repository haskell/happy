#ifndef QUALIFIEDPRELUDE
#define QUALIFIEDPRELUDE Prelude
#endif

{
module Main where

import Data.Char
import Control.Monad.Error
import System.Exit
import System.Environment (getProgName)
import Data.List (isPrefixOf)

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
            |       'T' 'Z' Exp   { $3 QUALIFIEDPRELUDE.+ 1 }
            |       'S' Exp       { $2 QUALIFIEDPRELUDE.+ 1 }

{

type ParseM a = QUALIFIEDPRELUDE.Either ParseError a
data ParseError
        = ParseError (QUALIFIEDPRELUDE.Maybe (Token, [QUALIFIEDPRELUDE.String]))
        | StringError QUALIFIEDPRELUDE.String
    deriving (QUALIFIEDPRELUDE.Eq,QUALIFIEDPRELUDE.Show)
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
	| TokenTest
    deriving (QUALIFIEDPRELUDE.Eq,QUALIFIEDPRELUDE.Show)

handleErrorExpList :: ([Token], [QUALIFIEDPRELUDE.String]) -> ParseM a
handleErrorExpList ([], _) = throwError QUALIFIEDPRELUDE.$ ParseError QUALIFIEDPRELUDE.Nothing
handleErrorExpList (ts, explist) = throwError QUALIFIEDPRELUDE.$ ParseError QUALIFIEDPRELUDE.$ QUALIFIEDPRELUDE.Just QUALIFIEDPRELUDE.$ (QUALIFIEDPRELUDE.head ts, explist)

lexer :: QUALIFIEDPRELUDE.String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c QUALIFIEDPRELUDE.== 'S'  = TokenSucc:(lexer cs)
    | c QUALIFIEDPRELUDE.== 'Z'  = TokenZero:(lexer cs)
    | c QUALIFIEDPRELUDE.== 'T'  = TokenTest:(lexer cs)
    | QUALIFIEDPRELUDE.otherwise = QUALIFIEDPRELUDE.error "lexer error"

main :: QUALIFIEDPRELUDE.IO ()
main = do
  test "Z Z" QUALIFIEDPRELUDE.$ QUALIFIEDPRELUDE.Left (ParseError (QUALIFIEDPRELUDE.Just (TokenZero,[])))
  test "T S" QUALIFIEDPRELUDE.$ QUALIFIEDPRELUDE.Left (ParseError (QUALIFIEDPRELUDE.Just (TokenSucc,["'Z'"])))

  where
    test inp exp = do
      QUALIFIEDPRELUDE.putStrLn QUALIFIEDPRELUDE.$ "testing " QUALIFIEDPRELUDE.++ inp
      let tokens = lexer inp
      when (parseFoo tokens QUALIFIEDPRELUDE./= exp) QUALIFIEDPRELUDE.$ do
        QUALIFIEDPRELUDE.print (parseFoo tokens)
        exitWith (ExitFailure 1)
}
