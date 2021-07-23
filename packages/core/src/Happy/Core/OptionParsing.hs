{-# LANGUAGE ExplicitForAll #-}

module Happy.Core.OptionParsing(
  parseOptions, beginOptionsWith, requireUnnamedArgument, OnNone(..), OnMultiple(..),
  removeAllOverlaps, removeLongOption, removeLongOverlaps, cleanShortOption, cleanShortOverlaps
) where

import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode(..))
import System.IO
import Control.Monad (liftM)
import Data.List (isSuffixOf, sort, sortBy, group, intersect, elemIndex, intersect, delete)
import Data.Ord
import Data.Maybe
import Data.Version (showVersion, Version)

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
parseOptions :: Eq cli => [OptDescr cli] -> Version -> [String] -> IO ([cli], [String])
parseOptions customOpts version args =
  let options = addFixed customOpts in do
  checkDuplicateOptions options
  case getOpt Permute options args of
    (cli, _, []) | elem (Fixed DumpVersion) cli ->
        bye (copyright version)

    (cli, _, []) | elem (Fixed DumpHelp) cli ->
        byeUsage options ""
    
    (cli, freeOpts, []) -> do
        let customFlags = [a | Custom a <- cli]
        return (customFlags, freeOpts)
   
    (_, _, errors) ->
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

-- Sort the options by the list of given short option characters.
-- Options which do not have any of these short option characters come after the sorted options which have been matched.
beginOptionsWith :: [Char] -> [OptDescr a] -> [OptDescr a]
beginOptionsWith chars opts = sortBy (comparing $ smallestIndex . shorts) matched ++ nonMatched
   where
      smallestIndex :: [Char] -> Int
      smallestIndex a = minimum (map (fromMaybe 1000 . flip elemIndex chars) a)
      matched = filter (not . null . intersect chars . shorts) opts
      nonMatched = filter (null . intersect chars . shorts) opts

-------- Option Conflicts --------

-- Combine removeLongOverlaps with cleanShortOverlaps
removeAllOverlaps :: [OptDescr a] -> [OptDescr b] -> [OptDescr b]
removeAllOverlaps a = removeLongOverlaps a . cleanShortOverlaps a

-- Remove a long option from the list of options, if existing.
-- This means: any option which features this long option string is removed (even if it has multiple different long option strings).
removeLongOption :: String -> [OptDescr a] -> [OptDescr a]
removeLongOption long opts = filter (not . elem long . longs) opts  

-- Remove all overlaps with the first list's long option strings from the second list (using removeLongOption).
removeLongOverlaps :: [OptDescr a] -> [OptDescr b] -> [OptDescr b]
removeLongOverlaps a b = foldr removeLongOption b (concatMap longs a)

-- Remove a short option from the list of options, if existing.
-- In contrast to `removeLongOption`, a matching short option character will just be removed
-- from its hosting option – the option itself remains, provided it features at least one other short or long option.
cleanShortOption :: Char -> [OptDescr a] -> [OptDescr a]
cleanShortOption short opts = filter nonempty $ map remove opts where
  remove (Option s a b c) = Option (delete short s) a b c

-- Remove all overlaps with the first list's short option strings from the second list (using cleanShortOption).
cleanShortOverlaps :: [OptDescr a] -> [OptDescr b] -> [OptDescr b]
cleanShortOverlaps a b = foldr cleanShortOption b (concatMap shorts a)

shorts :: OptDescr a -> [Char]
longs :: OptDescr a -> [String]
nonempty :: OptDescr a -> Bool
shorts (Option s _ _ _) = s
longs (Option _ l _ _) = l
nonempty (Option s l _ _) = not (null s) || not (null l)

-------- Internal helpers --------

checkDuplicateOptions :: [OptDescr a] -> IO ()
checkDuplicateOptions opts = case (multiples (concatMap shorts opts), multiples (concatMap longs opts)) of
   (x:_, _) -> die $ "Attention: option -" ++ x : " corresponds to multiple different arguments; please fix this.\n"
   (_, x:_) -> die $ "Attention: option --" ++ x ++ " corresponds to multiple different arguments; please fix this.\n"
   _        -> return ()
   where
      multiples :: Ord a => [a] -> [a]
      multiples = map head . filter ((> 1) . length) . group . sort

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

copyright :: Version -> String
copyright version = unlines [
 "Happy Version " ++ showVersion version ++ " Copyright (c) 1993-1996 Andy Gill, Simon Marlow (c) 1997-2005 Simon Marlow","",
 "Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.",
 "This program is free software; you can redistribute it and/or modify",
 "it under the terms given in the file 'LICENSE' distributed with",
 "the Happy sources."]

usageHeader :: [OptDescr a] -> String -> String
usageHeader opts prog = usageInfo header opts where
  header = "Usage: " ++ prog ++ " [OPTION...] file\n"

-- (Functor OptDescr) only exists since base-4.7.0.0, i.e. GHC 7.8.1
-- In the doc, it says (Functor OptDescr) exists since base-4.6.0.0, but that seems to be wrong
#if !MIN_VERSION_base(4,7,0)
instance Functor OptDescr where
    fmap f (Option a b argDescr c) = Option a b (fmap f argDescr) c

instance Functor ArgDescr where
    fmap f (NoArg a)    = NoArg (f a)
    fmap f (ReqArg g s) = ReqArg (f . g) s
    fmap f (OptArg g s) = OptArg (f . g) s
#endif