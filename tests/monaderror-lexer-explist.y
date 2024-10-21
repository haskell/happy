{
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
-- For ancient GHC 7.0.4
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Char
import Control.Monad (when)
import System.Exit
import System.Environment (getProgName)
import Data.List (isPrefixOf)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
}

%name parseFoo
%tokentype { Token }
%errorhandlertype explist
%error { handleErrorExpList }
%lexer { lexer } { TokenEOF }

%monad { ParseM } { (>>=) } { return }

%token
        'S'             { TokenSucc }
        'Z'             { TokenZero }
        'T'             { TokenTest }

%%

Exp         :       'Z'           { 0 }
            |       'T' 'Z' Exp   { $3 + 1 }
            |       'S' Exp       { $2 + 1 }

{

type ParseM a = ExceptT ParseError (State String) a
data ParseError = ParseError (Maybe (Token, [String]))
    deriving (Eq,Show)

data Token
        = TokenSucc
        | TokenZero
	| TokenTest
    | TokenEOF
    deriving (Eq,Show)

handleErrorExpList :: (Token, [String]) -> ParseM a
handleErrorExpList (t, explist) = throwE $ ParseError $ Just $ (t, explist)

lexer :: (Token -> ParseM a) -> ParseM a
lexer cont = do
  toks <- lift get
  case toks of
    [] -> cont TokenEOF
    c : rest -> do
      lift $ put rest
      if | isSpace c -> lexer cont
         | c == 'S'  -> cont TokenSucc
         | c == 'Z'  -> cont TokenZero
         | c == 'T'  -> cont TokenTest
         | otherwise -> error "lexer error"

main :: IO ()
main = do
  test "Z Z" $ Left (ParseError (Just (TokenZero,[])))
  test "T S" $ Left (ParseError (Just (TokenSucc,["'Z'"])))

  where
    test inp exp = do
      putStrLn $ "testing " ++ inp
      let act = evalState (runExceptT parseFoo) inp
      when (act /= exp) $ do
        print act
        exitWith (ExitFailure 1)
}
