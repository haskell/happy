{
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- For ancient GHC 7.0.4
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Monad (when)
import Data.IORef
import Data.Char
import System.Exit
}

%name parseStmts Stmts
%name parseExp Exp
%tokentype { LToken }
%error { \_ -> abort } { reportError } -- the entire point of this test
%error.expected                        -- as in monaderror-explist.y

%monad { ParseM } { (>>=) } { return }

%token
  '1' { (_, TOne) }
  '+' { (_, TPlus) }
  ';' { (_, TSemi) }
  '(' { (_, TOpen) }
  ')' { (_, TClose) }

%%

Stmts :: { [String] }
Stmts : {- empty -}           { [] }
      | Exp                   { [$1] }
      | Stmts ';' Exp         { $1 ++ [$3] }

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 ++ " + " ++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" ++ $2 ++ ")" }

{
data Token = TOne | TPlus | TSemi | TOpen | TClose
  deriving (Eq,Show)

----------- Validation monad
data Validate e a = V e (Maybe a)
  deriving Functor
instance Monoid e => Applicative (Validate e) where
  pure a = V mempty (Just a)
  V e1 f <*> V e2 a = V (e1 <> e2) (f <*> a)
instance Monoid e => Monad (Validate e) where
  V e Nothing   >>= _ = V e Nothing -- fatal
  V e1 (Just a) >>= k | V e2 b <- k a = V (e1 <> e2) b -- non-fatal

abort :: Monoid e => Validate e a -- this would be mzero from MonadPlus
abort = V mempty Nothing

recordError :: e -> Validate e () -- this would be tell from MonadWriter
recordError e = V e (Just ())

runValidate (V e mb_a) = (e, mb_a)
-----------

type ParseM = Validate [ParseError]

data ParseError
        = ParseError Int [String]
    deriving Eq
instance Show ParseError where
  show (ParseError loc exp) = "Parse error at " ++ locS ++ ". Expected: " ++ commaSep exp ++ "."
    where
      locS | loc < 0   = "EOF"
           | otherwise = "column " ++ show loc
      commaSep [] = ""
      commaSep [s] = s
      commaSep (s:ss) = s ++ "," ++ commaSep ss

recordParseError :: Int -> [String] -> ParseM ()
recordParseError loc expected = recordError [ParseError loc expected]

eofLoc :: Int
eofLoc = -1
reportError :: [LToken] -> [String] -> ([LToken] -> ParseM a) -> ParseM a
reportError ts expected resume = do
  let loc | (l,_):_ <- ts = l
          | otherwise     = eofLoc
  recordParseError loc expected
  resume ts

type LToken = (Int, Token) -- Token with location

lexer :: Int -> String -> [LToken]
lexer _ [] = []
lexer n (c:cs)
    | isSpace c = lexer (n+1) cs
    | c == '1'  = (n,TOne):(lexer (n+1) cs)
    | c == '+'  = (n,TPlus):(lexer (n+1) cs)
    | c == ';'  = (n,TSemi):(lexer (n+1) cs)
    | c == '('  = (n,TOpen):(lexer (n+1) cs)
    | c == ')'  = (n,TClose):(lexer (n+1) cs)
    | otherwise = error "lexer error"

main :: IO ()
main = do
  exit_code_ref <- newIORef ExitSuccess
  let exp_err loc = ParseError loc ["'1'","'('"]
  testStmts exit_code_ref "1+1;1" $ \(_,mb_ast) -> mb_ast == Just ["1 + 1", "1"]
  testStmts exit_code_ref "1++1;1" $ \(errs,_) -> errs == [exp_err 2]
  testStmts exit_code_ref "1++1;1;+" $ \(errs,_) -> errs == [exp_err 2, exp_err 7, exp_err eofLoc]
  testStmts exit_code_ref "11;1" $ \(errs,_) -> errs == [ParseError 1 ["';'"]]
  testStmts exit_code_ref "11;1;++" $ \(errs,_) -> errs == [ParseError 1 ["';'"], exp_err 5, exp_err 6, exp_err eofLoc]
  testStmts exit_code_ref "11;1;1++" $ \(errs,_) -> errs == [ParseError 1 ["';'"], exp_err 7, exp_err eofLoc]
  testExp exit_code_ref "11" $ \(errs,_) -> errs == [ParseError 1 ["'+'"]]

  testStmts exit_code_ref "(;1)" $ \(errs,mb_ast) -> errs == [exp_err 1, ParseError 3 ["';'"]]
  testStmts exit_code_ref "1+;" $ \(errs,mb_ast) -> errs == [exp_err 2, exp_err eofLoc]

  -- The main point of the following 2 tests: rather than discarding tokens until
  -- the EOF is reached upon the first error because of a missing ')', resume
  -- after ';'. In the first case, we error again at EOF, in the second case we error again on '+'
  testStmts exit_code_ref "(;" $ \(errs,mb_ast) ->
    errs   == [exp_err 1, exp_err eofLoc] &&
    mb_ast == Just ["catch","catch"]
  testStmts exit_code_ref "(;+1" $ \(errs,mb_ast) ->
    errs   == [exp_err 1, exp_err 2] &&
    mb_ast == Just ["catch","catch + 1"]

  readIORef exit_code_ref >>= exitWith
  where
    testStmts ref inp p = do
      putStrLn $ "testing Stmts " ++ inp
      let tokens = lexer 0 inp
      let res@(_,mb_ast) = runValidate $ parseStmts tokens
      when (not (p res) || mb_ast == Nothing) $ do -- mb_ast == Nothing: Ensure that we *never* fail to resume!
        print res
        writeIORef ref (ExitFailure 1)
    testExp ref inp p = do
      putStrLn $ "testing Exp " ++ inp
      let tokens = lexer 0 inp
      let res = runValidate $ parseExp tokens
      when (not (p res)) $ do
        print res
        writeIORef ref (ExitFailure 1)
}
