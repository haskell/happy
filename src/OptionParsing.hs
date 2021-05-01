module OptionParsing(parseOptions, CliOpts(..)) where

import FrontendCLI
import MiddleendCLI
import BackendCLI
import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode(..))
import System.IO
import Control.Monad (liftM)
import Data.List (isSuffixOf)
import Data.Version (showVersion)
import Data.Char
import Data.Maybe
import Paths_happy

data CliOpts ext = CliOpts {
  frontendOpts :: FrontendOpts,
  middleendOpts :: MiddleendOpts,
  backendOpts :: BackendOpts,
  exts :: [ext]
}

parseOptions :: Eq ext => [OptDescr ext] -> [String] -> IO (CliOpts ext)
parseOptions ext args =
  let allArgInfos = argInfoBase ++ map (fmap Ext) ext in
  case getOpt Permute allArgInfos args of
    (cli,_,[]) | DumpVersion `elem` cli ->
        bye copyright
    (cli,_,[]) | DumpHelp `elem` cli -> do
        prog <- getProgramName
        bye (usageInfo (usageHeader prog) allArgInfos)
    (cli,_,_) | OptDebugParser `elem` cli
                && OptArrayTarget `notElem` cli -> do
        die "Cannot use debugging without -a\n"
    (cli,[fileName],[]) -> do
        extractOptions cli fileName
    (_,_,errors) -> do
        prog <- getProgramName
        die (concat errors ++ usageInfo (usageHeader prog) allArgInfos)

extractOptions :: Eq ext => [CliFlag ext] -> String -> IO (CliOpts ext)
extractOptions cli fileName = do
  baseName <- getBaseName (reverse fileName)
  frontend <- getFrontendOpts cli fileName baseName
  middleend <- getMiddleendOpts cli fileName baseName
  backend <- getBackendOpts cli baseName
  return CliOpts { frontendOpts = frontend, middleendOpts = middleend, backendOpts = backend, exts = getExts cli }
  
getFrontendOpts :: Eq ext => [CliFlag ext] -> String -> String -> IO FrontendOpts
getFrontendOpts cli fileName baseName = do
  pretty_file <- getPrettyFileName baseName cli
  return FrontendOpts { file = fileName, prettyFile = pretty_file }
  
getMiddleendOpts :: Eq ext => [CliFlag ext] -> String -> String -> IO MiddleendOpts
getMiddleendOpts cli fileName baseName = do
  info_file <- getInfoFileName baseName cli
  return MiddleendOpts { inFile = fileName, infoFile = info_file }

getBackendOpts :: Eq ext => [CliFlag ext] -> String -> IO BackendOpts
getBackendOpts cli baseName = do
  out_file <- getOutputFileName baseName cli
  template <- getTemplate cli
  magic_name <- getMagicName cli
  target' <- getTarget cli
  coerce' <- getCoerce target' cli
  return BackendOpts {
    BackendCLI.outFile = out_file,
    templateDir = template,
    magicName = magic_name,
    strict = getStrict cli,
    ghc = getGhc cli,
    coerce = coerce',
    target = target',
    debug = getDebug cli,
    glr = getGLR cli,
    glrDecode = getGLRDecode cli,
    glrFilter = getGLRFilter cli
  }

getExts :: [CliFlag ext] -> [ext]
getExts = mapMaybe toExt where
   toExt (Ext a) = Just a
   toExt _       = Nothing

------------------------------------------------------------------------------
-- Old happy option parsing
data CliFlag ext =
                 DumpVersion
               | DumpHelp
               | OptInfoFile (Maybe String)
               | OptPrettyFile (Maybe String)
               | OptTemplate String
               | OptMagicName String

               | OptGhcTarget
               | OptArrayTarget
               | OptUseCoercions
               | OptDebugParser
               | OptStrict
               | OptOutputFile String
               | OptGLR
               | OptGLR_Decode
               | OptGLR_Filter

               | Ext ext
  deriving Eq

-- Base arguments, i.e. baseline frontend, middleend and backend.
argInfoBase :: [OptDescr (CliFlag ext)]
argInfoBase  = [
    Option ['o'] ["outfile"] (ReqArg OptOutputFile "FILE")
       "write the output to FILE (default: file.hs)",
    Option ['i'] ["info"] (OptArg OptInfoFile "FILE")
       "put detailed grammar info in FILE",
    Option ['p'] ["pretty"] (OptArg OptPrettyFile "FILE")
       "pretty print the production rules to FILE",
    Option ['t'] ["template"] (ReqArg OptTemplate "DIR")
       "look in DIR for template files",
    Option ['m'] ["magic-name"] (ReqArg OptMagicName "NAME")
       "use NAME as the symbol prefix instead of \"happy\"",
    Option ['s'] ["strict"] (NoArg OptStrict)
       "evaluate semantic values strictly (experimental)",
    Option ['g'] ["ghc"]    (NoArg OptGhcTarget)
       "use GHC extensions",
    Option ['c'] ["coerce"] (NoArg OptUseCoercions)
       "use type coercions (only available with -g)",
    Option ['a'] ["array"] (NoArg OptArrayTarget)
       "generate an array-based parser",
    Option ['d'] ["debug"] (NoArg OptDebugParser)
       "produce a debugging parser (only with -a)",
    Option ['l'] ["glr"] (NoArg OptGLR)
       "Generate a GLR parser for ambiguous grammars",
    Option ['k'] ["decode"] (NoArg OptGLR_Decode)
       "Generate simple decoding code for GLR result",
    Option ['f'] ["filter"] (NoArg OptGLR_Filter)
       "Filter the GLR parse forest with respect to semantic usage",
    Option ['?'] ["help"] (NoArg DumpHelp)
       "display this help and exit",
    Option ['V','v'] ["version"] (NoArg DumpVersion)   -- ToDo: -v is deprecated
       "output version information and exit"
  ]

------------------------------------------------------------------------------
-- Extract various command-line options.

getTarget :: Eq a => [CliFlag a] -> IO Target
getTarget cli = case [ t | (Just t) <- map optToTarget cli ] of
                       (t:ts) | all (==t) ts -> return t
                       []  -> return TargetHaskell
                       _   -> dieHappy "multiple target options\n"

getOutputFileName :: Eq a => String -> [CliFlag a] -> IO String
getOutputFileName base cli
       = case [ s | (OptOutputFile s) <- cli ] of
               []   -> return (base ++ ".hs")
               f:fs -> return (last (f:fs))

getInfoFileName :: Eq a => String -> [CliFlag a] -> IO (Maybe String)
getInfoFileName base cli
       = case [ s | (OptInfoFile s) <- cli ] of
               []      -> return Nothing
               [f]     -> case f of
                               Nothing -> return (Just (base ++ ".info"))
                               Just j  -> return (Just j)
               _many   -> dieHappy "multiple --info/-i options\n"

getPrettyFileName :: Eq a => String -> [CliFlag a] -> IO (Maybe String)
getPrettyFileName base cli
       = case [ s | (OptPrettyFile s) <- cli ] of
               []      -> return Nothing
               [f]     -> case f of
                               Nothing -> return (Just (base ++ ".grammar"))
                               Just j  -> return (Just j)
               _many   -> dieHappy "multiple --pretty/-p options\n"

getTemplate :: Eq a => [CliFlag a] -> IO (Maybe String)
getTemplate cli
       = case [ s | (OptTemplate s) <- cli ] of
               []         -> return Nothing
               f:fs       -> return $ Just (last (f:fs))

getMagicName :: Eq a => [CliFlag a] -> IO (Maybe String)
getMagicName cli
       = case [ s | (OptMagicName s) <- cli ] of
               []         -> return Nothing
               f:fs       -> return (Just (map toLower (last (f:fs))))

getCoerce :: Eq a => Target -> [CliFlag a] -> IO Bool
getCoerce _target cli
       = if OptUseCoercions `elem` cli
            then if OptGhcTarget `elem` cli
                       then return True
                       else dieHappy ("-c/--coerce may only be used " ++
                                      "in conjunction with -g/--ghc\n")
            else return False

getGhc :: Eq a => [CliFlag a] -> Bool
getGhc = elem OptGhcTarget

getStrict :: Eq a => [CliFlag a] -> Bool
getStrict = elem OptStrict

getDebug :: Eq a => [CliFlag a] -> Bool
getDebug = elem OptDebugParser

getGLR :: Eq a => [CliFlag a] -> Bool
getGLR = elem OptGLR

getGLRFilter :: Eq a => [CliFlag a] -> Bool
getGLRFilter = elem OptGLR_Filter

getGLRDecode :: Eq a => [CliFlag a] -> Bool
getGLRDecode = elem OptGLR_Decode

------------------------------------------------------------------------------
-- Helpers

optToTarget :: CliFlag a -> Maybe Target
optToTarget OptArrayTarget    = Just TargetArrayBased
optToTarget _                 = Nothing


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

------------------------------------------------------------------------------

getBaseName :: String -> IO String
getBaseName ('y':'l':'.':nm) = return (reverse nm)
getBaseName ('y':'.':nm)     = return (reverse nm)
getBaseName f                =
      dieHappy ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")

------------------------------------------------------------------------------

copyright :: String
copyright = unlines [
 "Happy Version " ++ showVersion version ++ " Copyright (c) 1993-1996 Andy Gill, Simon Marlow (c) 1997-2005 Simon Marlow","",
 "Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.",
 "This program is free software; you can redistribute it and/or modify",
 "it under the terms given in the file 'LICENSE' distributed with",
 "the Happy sources."]

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file\n"

-----------------------------------------------------------------------------