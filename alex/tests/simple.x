{
-- Tests the basic operation.
module Main where

import Array
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.Char
import System.Exit
}

%word = [A-Za-z]+

tokens :-

$white+			{ skip }

<0> "magic"		{ magic } -- should override later patterns
<0> %word $		{ eol }  -- test trailing context
<0> ^ %word		{ bol }  -- test left context
<0> %word		{ word }

<0> \(			{ begin parens }
<parens> [A-Za-z]+	{ parenword }
<parens> \)		{ begin 0 }

{
skip p c input len cont scs = cont scs

begin code p c input len cont (_,s) = cont (code,s)

{- we can now have comments in source code? -}
word p c input len cont scs = 
  take len input : cont scs

eol p c input len cont scs = 
  ("EOL:"++ take len input) : cont scs

bol p c input len cont scs = 
  ("BOL:"++ take len input) : cont scs

parenword p c input len cont scs = 
  map toUpper (take len input) : cont scs

magic p c input len cont scs =
  "PING!" : cont scs

stop p c "" scs   = ["stopped."]
stop p c rest scs = ["error."]

main = do
  let test1 = gscan stop () str1
  print test1
  when (test1 /= out1) $ exitFailure

  let test2 = gscan stop () str2
  print test2
  when (test2 /= out2) $ exitFailure

str1 = "a b c (d e f) magic (magic) eol\nbol"
out1 = ["BOL:a","b","c","D","E","F","PING!","MAGIC","EOL:eol", "BOL:bol", "stopped."]

str2 = "."
out2 = ["error."]
}
