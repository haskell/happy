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

module Main (main) where

import Info
import Parser
import Scan
import CharSet
import System
import AbsSyn
import DFA
import Util
import qualified Alex

import System.Console.GetOpt
import Data.Char
import Data.Array
import Data.FiniteMap
import System.IO
import Control.Monad

version = "2.0"

-- `main' decodes the command line arguments and calls `alex'.  

main:: IO ()
main =	do
 args <- getArgs
 case getOpt Permute argInfo args of
    (cli,[file],[]) -> 
	runAlex cli file
    (cli,[],[]) | DumpVersion `elem` cli -> do
	putStrLn copyright
	exitWith ExitSuccess
    (_,_,errors) -> do
	prog <- getProgName
        die (concat errors ++ usageInfo (usageHeader prog) argInfo)

copyright = "Alex version " ++ version ++ ", (c) 2003 Chris Dornan and Simon Marlow\n"

usageHeader prog = prog ++ " [OPTION...] file"

runAlex cli file = do
  basename <- case (reverse file) of
		'x':'.':r -> return (reverse r)
		_         -> die (file ++ ": filename must end in \'.x\'\n")
  
  o_file <- case [ f | OptOutputFile f <- cli ] of
		[]  -> return (basename ++ ".hs")
		[f] -> return f
		_   -> dieAlex "multiple -o/--outfile options"
  
  prg <- readFile file

  case unP (parse (lexer prg)) initialParserEnv of
	Right (_,script) -> do
		(put_info, finish_info) <- 
		   case [ f | OptInfoFile f <- cli ] of
			[]  -> return (\_ -> return (), return ())
			[Nothing] -> infoStart file (basename ++ ".info")
			[Just f]  -> infoStart file f
			_   -> dieAlex "multiple -i/--info options"

		out_h <- openFile o_file WriteMode

		let
			(script', scs, sc_hdr) = encode_start_codes "" script

			go n (DefScanner scr) = do
				let dfa = scanner2dfa scr scs
				    nm  = scannerName scr
				put_info (infoDFA n nm dfa "")
				hPutStr out_h (outputDFA n nm dfa "")
			go n (DefCode code) =
			  	hPutStr out_h code

		zipWithM_ go [1..] script'
		hPutStr out_h (sc_hdr "")
		hClose out_h
		finish_info

	Left (Just (Alex.Pn _ line col),err) -> 
		die (file ++ ":" ++ show line ++ ":" ++ show col
				 ++ ": " ++ err ++ "\n")
	Left (Nothing, err) ->
		die (file ++ ": " ++ err ++ "\n")

infoStart x_file info_file = do
  h <- openFile info_file WriteMode
  infoHeader h x_file
  return (hPutStr h, hClose h)

infoHeader h file = do
  hPutStrLn h ("Info file produced by Alex version " ++ version ++ 
		", from " ++ file)
  hPutStrLn h hline
  hPutStr h "\n"

initialParserEnv :: (FiniteMap String CharSet, FiniteMap String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv = listToFM [("white", charSet " \t\n\v\f\r"),
		       ("printable", charSet [chr 32 .. chr 126]),
		       (".", charSetComplement emptyCharSet 
				`charSetMinus` charSetSingleton '\n')]
initREEnv = emptyFM

-- -----------------------------------------------------------------------------
-- Command-line flags

data CLIFlags 
  = OptDebug
--TODO:
--  | OptGhcTarget
  | OptOutputFile FilePath
  | OptInfoFile (Maybe FilePath)
  | DumpVersion
  deriving Eq

argInfo :: [OptDescr CLIFlags]
argInfo  = [
   Option ['d'] ["debug"] (NoArg OptDebug)
	"Produce a debugging parser (only with -a)",
-- TODO:
--   Option ['g'] ["ghc"]    (NoArg OptGhcTarget)
--	"Use GHC extensions",
   Option ['o'] ["outfile"] (ReqArg OptOutputFile "FILE")
	"Write the output to FILE (default: file.hs)",
   Option ['i'] ["info"] (OptArg OptInfoFile "FILE")
	"Put detailed grammar info in FILE",
   Option ['v'] ["version"] (NoArg DumpVersion)
      "Print out version info"
  ]

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
   
    outputAcc (Acc prio act lctx rctx)
	= str "Acc " . shows prio . space
	. paren (str act) . space
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

die :: String -> IO a
die s = do 
  hPutStr stderr s
  exitWith (ExitFailure 1)

dieAlex :: String -> IO a
dieAlex s = do
  prog <- getProgName
  hPutStr stderr (prog ++ ": " ++ s)
  exitWith (ExitFailure 1)

