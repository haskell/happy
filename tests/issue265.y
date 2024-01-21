{
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- For ancient GHC 7.0.4
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Monad (when)
import Data.Char
import System.Exit
}

%name parseStmts
%tokentype { Token }
%errorhandlertype explist
%error { handleError }

%monad { ParseM } { (>>=) } { return }

%token
  '1' { TOne }
  '+' { TPlus }
  ';' { TSemi }

%%

Stmts : {- empty -}    { [] }
      | Stmt           { [$1] }
      | Stmts ';' Stmt { $1 ++ [$3] }

Stmt : Exp { ExpStmt $1 }

Exp : '1'                { One }
    | Exp '+' Exp %shift { Plus $1 $3 }

{
data Token = TOne | TPlus | TSemi
  deriving (Eq,Show)

type Stmts = [Stmt]
data Stmt = ExpStmt Exp
  deriving (Eq, Show)
data Exp = One | Plus Exp Exp
  deriving (Eq, Show)

type ParseM = Either ParseError

data ParseError
        = ParseError [String]
    deriving Eq
instance Show ParseError where
  show (ParseError exp) = "Parse error. Expected: " ++ show exp

recordParseError :: [String] -> ParseM a
recordParseError expected = Left (ParseError expected)

handleError :: ([Token], [String]) -> ParseM a
handleError (ts, expected) = recordParseError expected

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c == '1'  = TOne:(lexer cs)
    | c == '+'  = TPlus:(lexer cs)
    | c == ';'  = TSemi:(lexer cs)
    | otherwise = error "lexer error"

main :: IO ()
main = do
  test "11;1" $ \res -> res == Left (ParseError ["';'","'+'"])
  where
    test inp p = do
      putStrLn $ "testing " ++ inp
      let tokens = lexer inp
      let res = parseStmts tokens
      when (not (p res)) $ do
        print res
        exitWith (ExitFailure 1)
}
