{
-- Tests the basic operation.
module Main where

import Data.Char (toUpper)
import Control.Monad
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
{- we can now have comments in source code? -}
word (p,input) len = return (take len input)

eol (p,input) len = return ("EOL:"++ take len input)

bol (p,input) len = return ("BOL:"++ take len input)

parenword (p,input) len = return (map toUpper (take len input))

magic (p,input) len = return "PING!"

alexEOF (p,"")   = return "stopped."
alexEOF (p,rest) = return "error."

scanner str = runAlex str $ do
  let loop = do tok <- alexScan; 
		if tok == "stopped." || tok == "error." 
			then return [tok]
			else do toks <- loop
				return (tok:toks)
  loop  

main = do
  let test1 = scanner str1
  print test1
  when (test1 /= out1) $ exitFailure

  let test2 = scanner str2
  print test2
  when (test2 /= out2) $ exitFailure

str1 = "a b c (d e f) magic (magic) eol\nbol"
out1 = ["BOL:a","b","c","D","E","F","PING!","MAGIC","EOL:eol", "BOL:bol", "stopped."]

str2 = "."
out2 = ["error."]
}
