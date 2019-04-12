module ParseMonad where

import           Control.Monad.Reader

type ParseResult = Either String
type P a = ReaderT (String, Int) ParseResult a

failP :: String -> P a
failP str = ReaderT (\_ -> Left str)

mkP :: (String -> Int -> ParseResult a) -> P a
mkP = ReaderT . uncurry

runP :: P a -> String -> Int -> ParseResult a
runP f s l = runReaderT f (s, l)

lineP :: P Int
lineP = asks snd
