{------------------------------------------------------------------------------
				     Alex

`main'/`alex' is the lexical analyser generator.  It reads and parses the lx
program from its input file and places a Haskell module in the output file.
The function exported by the output module may be used with the packages in the
`Scan' module to build parsers.

The `Sort' and `Map' modules contain generic utilies for sorting and
maintaining finite maps.  `Scan' contains basic definitions and utilities for
scanning the source script.  `RExp' defines the abstract syntax of regular
expression and scanners.  `DFA' provides `scanner2dfa' for converting scanners
to DFAs.

Chris Dornan, Aug-95, 10-Jul-96, 29-Sep-97, 11-Apr-00
------------------------------------------------------------------------------}

module Main where

import Parser
import Scan
import CharSet
import System
import Alex
import AbsSyn
import DFA

import Data.Char
import Data.Array
import Data.FiniteMap
import System.IO

-- `main' decodes the command line arguments and calls `alex'.  

main:: IO ()
main =	getArgs							>>= \args ->
	check_flags args					>>
	case args of
	  [fn]     -> check_fn fn >>= \fn' -> alex fn fn'
	  [fn,fn'] -> check_fn fn >>= \_   -> alex fn fn'
	  _ -> usage
	where
	check_flags (('-':_):_) = usage
	check_flags _ = return ()

	usage = getProgName					>>= \prog ->
		die (vrn_ln ++ usg_ln prog)
	
	vrn_ln = "Alex 2.0 by Chris Dornan and Simon Marlow\n\n"

	usg_ln prog = "usage: " ++ prog ++ " <source_file> [<target_file>]\n"

	ext rbs = reverse ('s':'h':'.':rbs)

	ext_msg = ": only the .x extension is recognised\n"

	check_fn fn = 
		case reverse fn of
		  'x':'.':rbs -> return (ext rbs)
		  _           -> die (fn ++ ext_msg)

initialParserEnv :: (FiniteMap String CharSet, FiniteMap String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv = listToFM [("white", charSet " \t\n\v\f\r"),
		       ("printable", charSet [chr 32 .. chr 126]),
		       (".", charSetComplement emptyCharSet 
				`charSetMinus` charSetSingleton '\n')]
initREEnv = emptyFM

alex:: String -> String -> IO ()
alex src target =
	readFile src						>>= \prg ->
	case unP (parse (lexer prg)) initialParserEnv of
	  Right (_,script) ->
		writeFile target (foldr fmt (sc_hdr "") (zip [0..] script'))
		where
		(script', sc_hdr) = encode_start_codes ind script
	  Left (Just (Pn _ line col),err) -> 
		die (src ++ ":" ++ show line ++ ":" ++ show col
				 ++ ": " ++ err ++ "\n")
	  Left (Nothing, err) ->
		die (src ++ ": " ++ err ++ "\n")
	where
	fmt (n, DefScanner scr)
	  = outputDFA n (scannerName scr) (scanner2dfa scr)
	fmt (n, DefCode code)
	  = showString code

	ind = ""

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

-- -----------------------------------------------------------------------------
-- Printing the output

outputDFA :: Int -> String -> DFA Code -> ShowS
outputDFA n func_nm dfa
  = str func_nm . str " = " . outputDFAArr dfa
  where    
    outputDFAArr dfa
	= str "Array.array " . shows (bounds dfa) . space
	. char '['
	. interleave_shows (char ',') 
	     (map (\e -> str "\n\t" . outputDFAElem e) (assocs dfa))
	. str "]\n"

    outputDFAElem (i,s) = paren (shows i . char ',' . outputState s)

    outputState :: State Code -> ShowS
    outputState (St cl accs df out)
	= str "St " . shows cl . space
	. outputAccs accs . space
	. paren (shows df) . space
	. paren (outputArr out)

    outputAccs :: [Accept Code] -> ShowS
    outputAccs accs
	= brack (interleave_shows (char ',') (map (paren.outputAcc) accs))
   
    outputAcc (Acc prio act scs lctx rctx)
	= str "Acc " . shows prio . space
	. paren (str act) . space
	. shows scs . space
	. outputLCtx lctx . space
	. shows rctx

    outputLCtx Nothing
	= str "Nothing"
    outputLCtx (Just set)
	= str "Just " . paren (outputArr (charSetToArray set))

    outputArr arr
	= str "Array.array " . shows (bounds arr) . space
	. shows (assocs arr)

-- -----------------------------------------------------------------------------
-- Utils

str = showString
char c = (c :)

paren s = char '(' . s . char ')'
brack s = char '[' . s . char ']'

interleave_shows s [] = id
interleave_shows s xs = foldr1 (\a b -> a . s . b) xs

space = char ' '
nl = char '\n'

