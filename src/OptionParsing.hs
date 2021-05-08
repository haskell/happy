module OptionParsing(parseOptions, Flag(..), requireUnnamedArgument, OnNone(..), OnMultiple(..)) where

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

fixedOpts :: [OptDescr (Flag cli)]
fixedOpts = [
   Option "?" ["help"] (NoArg (Fixed DumpHelp)) "display this help and exit",
   Option "Vv" ["version"] (NoArg (Fixed DumpVersion)) "output version information and exit"
  ]

addFixed :: [OptDescr cli] -> [OptDescr (Flag cli)]
addFixed customOpts = map (fmap Custom) customOpts ++ fixedOpts

-------- Parsing --------

-- Returns all matched flags and the list of unnamed arguments
parseOptions :: Eq cli => [OptDescr cli] -> [String] -> IO ([cli], [String])
parseOptions customOpts args =
  let options = addFixed customOpts in
  case getOpt Permute options args of
    (cli, _, []) | elem (Fixed DumpVersion) cli ->
        bye copyright

    (cli, _, []) | elem (Fixed DumpHelp) cli -> do
        byeUsage options ""
    
    (cli, freeOpts, []) -> do
        let customFlags = [a | Custom a <- cli]
        return (customFlags, freeOpts)
   
    (_, _, errors) -> do
        dieUsage options (concat errors)

-- Requires the list of unnamed arguments to consist of at least one element, else show usage info and optionally an error.
requireUnnamedArgument :: [String] -> [OptDescr a] -> OnNone -> OnMultiple -> IO String
requireUnnamedArgument args customOpts onNone onMultiple =
   let options = addFixed customOpts in
   case (length args, onNone, onMultiple) of
      (0, DieUsage0, _)          -> dieUsage options ""
      (0, DieError0, IsOkayMult) -> dieUsage options "Provide at least one unnamed argument.\n"
      (0, DieError0, _)          -> dieUsage options "Provide one unnamed argument.\n"
      (1, _, _)                  -> return (head args)
      (_, _, IsOkayMult)         -> return (head args)
      (_, _, DieUsageMult)       -> dieUsage options ""
      (_, _, DieErrorMult)       -> dieUsage options "Provide exactly one unnamed argument.\n"

data OnNone = DieUsage0 | DieError0
data OnMultiple = IsOkayMult | DieUsageMult | DieErrorMult

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

dieUsage :: [OptDescr a] -> String -> IO b
dieUsage opts s = getProgramName >>= die . (s ++) . usageHeader opts

byeUsage :: [OptDescr a] -> String -> IO b
byeUsage opts s = getProgramName >>= bye . (s ++) . usageHeader opts

copyright :: String
copyright = unlines [
 "Happy Version " ++ showVersion version ++ " Copyright (c) 1993-1996 Andy Gill, Simon Marlow (c) 1997-2005 Simon Marlow","",
 "Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.",
 "This program is free software; you can redistribute it and/or modify",
 "it under the terms given in the file 'LICENSE' distributed with",
 "the Happy sources."]

usageHeader :: [OptDescr a] -> String -> String
usageHeader opts prog = usageInfo header opts where
  header = "Usage: " ++ prog ++ " [OPTION...] file\n"