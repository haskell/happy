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
            |       'T' 'Z' Exp   { $3 + 1 }
            |       'S' Exp       { $2 + 1 }

{

type ParseM a = Either ParseError a
data ParseError
        = ParseError (Maybe (Token, [String]))
        | StringError String
    deriving (Eq,Show)
instance Error ParseError where
    strMsg = StringError

data Token
        = TokenSucc
        | TokenZero
	| TokenTest
    deriving (Eq,Show)

handleErrorExpList :: ([Token], [String]) -> ParseM a
handleErrorExpList ([], _) = throwError $ ParseError Nothing
handleErrorExpList (ts, explist) = throwError $ ParseError $ Just $ (head ts, explist)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c == 'S'  = TokenSucc:(lexer cs)
    | c == 'Z'  = TokenZero:(lexer cs)
    | c == 'T'  = TokenTest:(lexer cs)
    | otherwise = error "lexer error"

main :: IO ()
main = do
  test "Z Z" $ Left (ParseError (Just (TokenZero,[])))
  test "T S" $ Left (ParseError (Just (TokenSucc,["'Z'"])))

  where
    test inp exp = do
      putStrLn $ "testing " ++ inp
      let tokens = lexer inp
      when (parseFoo tokens /= exp) $ do
        print (parseFoo tokens)
        exitWith (ExitFailure 1)
}
