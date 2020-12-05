{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ParseMonad.Bootstrapped where

import Control.Monad.Reader
import ParseMonad

type P = ReaderT (String, Int) ParseResult

mkP :: (String -> Int -> ParseResult a) -> P a
mkP = ReaderT . uncurry

runP :: P a -> String -> Int -> ParseResult a
runP f s l = runReaderT f (s, l)

instance ParseMonad P where
  failP mkStr = ReaderT (\(_, l) -> Left $ mkStr l)
  lineP = asks snd
  runFromStartP m s l = runP m s l

lexTokenP :: HasLexer token => (token -> P r) -> P r
lexTokenP k = ReaderT $ uncurry $ lexToken (\t -> runP $ k t)
