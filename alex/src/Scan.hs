{-# OPTIONS -cpp #-}
module Scan(lexer, AlexPosn(..), Token(..), Tkn(..), tokPosn) where

import Data.Char
import Debug.Trace
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
