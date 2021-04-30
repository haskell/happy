module ParseMonad.Class where

type Pfunc a = String -> Int -> ParseResult a

class HasLexer token where
  lexToken :: (token -> Pfunc r) -> Pfunc r

type ParseResult = Either String

class Monad p => ParseMonad p where
  failP :: (Int -> String) -> p a
  lineP :: p Int
  runFromStartP :: p a -> String -> Int -> ParseResult a
