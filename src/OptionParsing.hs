module OptionParsing(parseOptions, Flag(..), getBaseName) where

import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode(..))
import System.IO
import Control.Monad (liftM)
import Data.List (isSuffixOf)
import Data.Version (showVersion)
import Paths_happy

-------- Flag definitions and standard options --------

data Flag cli =
   Fixed FixedFlag | -- Flags that are always present in any happy
   Custom cli        -- Flags that come from the specific packages that are used
  deriving Eq

data FixedFlag =
   DumpVersion |
   DumpHelp
  deriving Eq

fixedOptions :: [OptDescr (Flag cli)]
fixedOptions = [
   Option "?" ["help"] (NoArg (Fixed DumpHelp)) "display this help and exit",
   Option "Vv" ["version"] (NoArg (Fixed DumpVersion)) "output version information and exit"
  ]

-------- Parsing --------

parseOptions :: Eq cli => [OptDescr cli] -> [String] -> IO ([cli], String)
parseOptions customOptions args =
  let allOptions = map (fmap Custom) customOptions ++ fixedOptions in
  case getOpt Permute allOptions args of
    (cli,_,[]) | elem (Fixed DumpVersion) cli ->
        bye copyright

    (cli,_,[]) | elem (Fixed DumpHelp) cli -> do
        prog <- getProgramName
        bye (usageInfo (usageHeader prog) allOptions)
    
    (cli,[fileName],[]) -> do
        let customFlags = [a | Custom a <- cli]
        return (customFlags, fileName) 
   
    (_,_,errors) -> do
        prog <- getProgramName
        die (concat errors ++ usageInfo (usageHeader prog) allOptions)

-------- Helpers --------

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str' `withoutSuffix` suff
            | suff `isSuffixOf` str' = take (length str' - length suff) str'
            | otherwise              = str'

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

dieHappy :: String -> IO a
dieHappy s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)

copyright :: String
copyright = unlines [
 "Happy Version " ++ showVersion version ++ " Copyright (c) 1993-1996 Andy Gill, Simon Marlow (c) 1997-2005 Simon Marlow","",
 "Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.",
 "This program is free software; you can redistribute it and/or modify",
 "it under the terms given in the file 'LICENSE' distributed with",
 "the Happy sources."]

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file\n"

-- Get file name without extension – exit if file doesn't end in .y or .ly
-- Todo: this (and filename as required unnamed argument) is frontend-specific and doesn't belong here
getBaseName :: String -> IO String
getBaseName = getBaseName' . reverse where
  getBaseName' ('y':'l':'.':nm) = return (reverse nm)
  getBaseName' ('y':'.':nm)     = return (reverse nm)
  getBaseName' f                = dieHappy ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")