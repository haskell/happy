{-# OPTIONS -fglasgow-exts #-}
-- -----------------------------------------------------------------------------
-- 
-- Main.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- ----------------------------------------------------------------------------}

module Main (main) where

import Output
import Info
import Parser
import Scan
import CharSet
import System
import AbsSyn
import DFA
import Util

import System.Console.GetOpt
import Data.Char
import Data.List
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
  
  prg <- readFile file

  case unP (parse (lexer prg)) initialParserEnv of
	Left (Just (AlexPn _ line col),err) -> 
		die (file ++ ":" ++ show line ++ ":" ++ show col
				 ++ ": " ++ err ++ "\n")
	Left (Nothing, err) ->
		die (file ++ ": " ++ err ++ "\n")

	Right (_,script) -> alex cli file basename script


alex cli file basename script = do
   (put_info, finish_info) <- 
      case [ f | OptInfoFile f <- cli ] of
 	   []  -> return (\_ -> return (), return ())
 	   [Nothing] -> infoStart file (basename ++ ".info")
 	   [Just f]  -> infoStart file f
 	   _   -> dieAlex "multiple -i/--info options"
   
   o_file <- case [ f | OptOutputFile f <- cli ] of
		[]  -> return (basename ++ ".hs")
		[f] -> return f
		_   -> dieAlex "multiple -o/--outfile options"
  
   let target 
	| OptGhcTarget `elem` cli = GhcTarget
	| otherwise               = HaskellTarget

   let template_name = templateFile target cli
		
   out_h <- openFile o_file WriteMode
   
   let
	 (maybe_header, scanner, maybe_footer) = script
 	 (scanner', scs, sc_hdr) = encode_start_codes "" scanner
 
   hPutStr out_h (optsToInject target cli)
   case maybe_header of
	Nothing   -> return ()
	Just code -> hPutStr out_h code

   hPutStr out_h (importsToInject target cli)

   let dfa = scanner2dfa scanner' scs
       nm  = scannerName scanner'

   put_info (infoDFA 1 nm dfa "")
   hPutStr out_h (outputDFA target 1 nm dfa "")

   case maybe_footer of
	Nothing   -> return ()
	Just code -> hPutStr out_h code

   hPutStr out_h (sc_hdr "")

   -- add the template
   tmplt <- readFile template_name
   hPutStr out_h tmplt

   hClose out_h
   finish_info

optsToInject :: Target -> [CLIFlags] -> String
optsToInject target cli
   | GhcTarget <- target = "{-# OPTIONS -fglasgow-exts -cpp #-}\n"
   | otherwise		 = "{-# OPTIONS -cpp #-}\n"

importsToInject :: Target -> [CLIFlags] -> String
importsToInject tgt cli = always_imports ++ debug_imports ++ glaexts_import
  where
	glaexts_import | OptGhcTarget `elem` cli    = import_glaexts
		       | otherwise                  = ""

	debug_imports  | OptDebugParser `elem` cli = import_debug
		       | otherwise		   = ""

-- CPP is turned on for -fglasogw-exts, so we can use conditional compilation:

always_imports = "#if __GLASGOW_HASKELL__ >= 503\n\ 
		   \import Data.Array\n\ 
		   \import Data.Char (ord)\n\ 
		   \import Data.Array.Base (unsafeAt)\n\ 
		   \#else\n\ 
		   \import Array\n\ 
		   \import Char (ord)\n\ 
		   \#endif\n"

import_glaexts = "#if __GLASGOW_HASKELL__ >= 503\n\ 
		   \import GHC.Exts\n\ 
		   \#else\n\ 
		   \import GlaExts\n\ 
		   \#endif\n"

import_debug = "#if __GLASGOW_HASKELL__ >= 503\n\ 
		   \import System.IO\n\ 
		   \import System.IO.Unsafe\n\ 
		   \import Debug.Trace\n\ 
		   \#else\n\ 
		   \import IO\n\ 
		   \import IOExts\n\ 
		   \#endif\n"

templateFile target cli
  = dir ++ "/AlexTemplate" ++ maybe_monad ++ maybe_ghc ++ maybe_debug
  where 
	dir = case [ d | OptTemplateDir d <- cli ] of
			[] -> "."
			ds -> last ds

	maybe_monad
	  | OptMonad `elem` cli = "-monad"
	  | otherwise           = ""

	maybe_ghc 
	  | GhcTarget <- target  = "-ghc"
	  | otherwise            = ""

	maybe_debug
	  | OptDebugParser `elem` cli  = "-debug"
	  | otherwise		       = ""

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
  = OptDebugParser
  | OptGhcTarget
  | OptOutputFile FilePath
  | OptInfoFile (Maybe FilePath)
  | OptTemplateDir FilePath
  | OptMonad
  | DumpVersion
  deriving Eq

argInfo :: [OptDescr CLIFlags]
argInfo  = [
   Option ['d'] ["debug"] (NoArg OptDebugParser)
	"Produce a debugging scanner",
   Option ['g'] ["ghc"]    (NoArg OptGhcTarget)
	"Use GHC extensions",
   Option ['o'] ["outfile"] (ReqArg OptOutputFile "FILE")
	"Write the output to FILE (default: file.hs)",
   Option ['i'] ["info"] (OptArg OptInfoFile "FILE")
	"Put detailed state-machine info in FILE",
   Option ['t'] ["template"] (ReqArg OptTemplateDir "DIR")
	"Look in DIR for template files",
   Option ['m'] ["monad"] (NoArg OptMonad)
	"Use the monad-based scanner",
   Option ['v'] ["version"] (NoArg DumpVersion)
      "Print out version info"
  ]

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

