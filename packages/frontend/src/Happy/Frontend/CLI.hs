module Happy.Frontend.CLI(Flag(..), options, parseFlags, parseAndRun, getBaseName) where

import Happy.Frontend
import Happy.Core.Grammar
import Happy.Core.GenUtils
import System.Console.GetOpt

-------- CLI flags and options --------

data Flag =
    OptPrettyFile (Maybe String) |
    DumpMangle
  deriving Eq

options :: [OptDescr Flag]
options = [
    Option "p" ["pretty"] (OptArg OptPrettyFile "FILE") "pretty print the production rules to FILE"

#ifdef DEBUG
  , Option "" ["mangle"] (NoArg DumpMangle) "Dump mangled input"
#endif
  
  ]

-------- [Flag] to FrontendArgs conversion --------

parseAndRun :: [Flag] -> String -> String -> IO (Either String Grammar)
parseAndRun flags filename basename = (parseFlags flags filename basename) >>= runFrontend

parseFlags :: [Flag] -> String -> String -> IO FrontendArgs
parseFlags flags filename baseName = do
  prettyName <- getPrettyFileName baseName flags
  return FrontendArgs {
    file = filename,
    prettyFile = prettyName,
    dumpMangle = DumpMangle `elem` flags
  }

getPrettyFileName :: String -> [Flag] -> IO (Maybe String)
getPrettyFileName baseName cli = case [ s | (OptPrettyFile s) <- cli ] of
  []        -> return Nothing
  [f] -> case f of
    Nothing -> return (Just (baseName ++ ".grammar"))
    Just j  -> return (Just j)
  _many     -> dieHappy "multiple --pretty/-p options\n"

-------- Exported helpers --------

-- Get filename without extension – exit if extension is neither .y nor .ly
getBaseName :: String -> IO String
getBaseName = getBaseName' . reverse where
  getBaseName' ('y':'l':'.':nm) = return (reverse nm)
  getBaseName' ('y':'.':nm)     = return (reverse nm)
  getBaseName' f                = dieHappy ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")