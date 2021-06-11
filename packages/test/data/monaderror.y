{
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Control.Monad (when)
import Data.Char
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
            |       'S' Exp     { $2 + 1 }

{

type ParseM a = Either ParseError a
data ParseError
        = ParseError (Maybe Token)
        | StringError String
    deriving (Eq,Show)
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
    deriving (Eq,Show)

handleError :: [Token] -> ParseM a
handleError [] = throwError $ ParseError Nothing
handleError ts = throwError $ ParseError $ Just $ head ts

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c == 'S'  = TokenSucc:(lexer cs)
    | c == 'Z'  = TokenZero:(lexer cs)
    | otherwise = error "lexer error"

main :: IO ()
main = do
    let tokens = lexer "S S"
    when (parseFoo tokens /= Left (ParseError Nothing)) $ do
        print (parseFoo tokens)
        exitWith (ExitFailure 1)

---
class Error a where
    noMsg :: a
    noMsg = strMsg ""
    strMsg :: String -> a
class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
instance MonadError e (Either e) where
    throwError = Left
}
