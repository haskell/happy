#ifndef QUALIFIEDPRELUDE
#define QUALIFY(X) X
#else
#define QUALIFY(X) QUALIFIEDPRELUDE.X
#endif

{
{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
module Main where

import Data.Char
import Control.Monad (when)
import System.Exit
import System.Environment (getProgName)
import Data.List (isPrefixOf)

}

%name parseFoo
%tokentype { Token }
%errorhandlertype explist
%error { handleErrorExpList }

%monad { ParseM } { (QUALIFY(>>=)) } { QUALIFY(return) }

%token
        'S'             { TokenSucc }
        'Z'             { TokenZero }
        'T'             { TokenTest }

%%

Exp         :       'Z'           { 0 }
            |       'T' 'Z' Exp   { $3 QUALIFY(+) 1 }
            |       'S' Exp       { $2 QUALIFY(+) 1 }

{

type ParseM a = QUALIFY(Either) ParseError a
data ParseError
        = ParseError (QUALIFY(Maybe) (Token, [QUALIFY(String])))
        | StringError QUALIFY(String)
    deriving (QUALIFY(Eq), QUALIFY(Show))
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
	| TokenTest
    deriving (QUALIFY(Eq), QUALIFY(Show))

handleErrorExpList :: ([Token], [QUALIFY(String])) -> ParseM a
handleErrorExpList ([], _) = throwError QUALIFY($) ParseError QUALIFY(Nothing)
handleErrorExpList (ts, explist) = throwError QUALIFY($) ParseError QUALIFY($) QUALIFY(Just) QUALIFY($) (QUALIFY(head) ts, explist)

lexer :: QUALIFY(String) -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c QUALIFY(==) 'S'  = TokenSucc:(lexer cs)
    | c QUALIFY(==) 'Z'  = TokenZero:(lexer cs)
    | c QUALIFY(==) 'T'  = TokenTest:(lexer cs)
    | QUALIFY(otherwise) = QUALIFY(error) "lexer error"

main :: QUALIFY(IO) ()
main = do
  test "Z Z" QUALIFY($) QUALIFY(Left) (ParseError (QUALIFY(Just) (TokenZero,[])))
  test "T S" QUALIFY($) QUALIFY(Left) (ParseError (QUALIFY(Just) (TokenSucc,["'Z'"])))

  where
    test inp exp = do
      QUALIFY(putStrLn) QUALIFY($) "testing " QUALIFY(++) inp
      let tokens = lexer inp
      when (parseFoo tokens QUALIFY(/=) exp) QUALIFY($) do
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
