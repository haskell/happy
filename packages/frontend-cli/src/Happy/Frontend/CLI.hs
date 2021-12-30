module Happy.Frontend.CLI(Flag(..), options, execFrontendCli, getBaseName) where

import Happy.Frontend
import Happy.Frontend.Mangler
import Happy.Frontend.PrettyGrammar
import Happy.Grammar
import Control.Monad (when, liftM)
import Data.List (isSuffixOf)
import System.Console.GetOpt
import System.Environment (getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO

data Flag =
    OptPrettyFile (Maybe String) |
    DumpMangle
  deriving Eq

options :: [OptDescr Flag]
options = [
    Option "p" ["pretty"] (OptArg OptPrettyFile "FILE") "pretty print the production rules to FILE",
    Option "" ["ddump-mangle"] (NoArg DumpMangle) "Dump mangled input"
  ]

execFrontendCli :: [Flag] -> String -> IO Grammar
execFrontendCli flags fl_name = do
  -- Parse flags
  basename <- getBaseName fl_name
  prettyFilename <- getPrettyFileName basename flags

  -- Open the file.

  fl <- readFile fl_name
  (_, file) <- case fileNameAndType fl_name of
      Nothing -> die ("`" ++ fl_name ++ "' does not end in `.y' or `.ly'\n")
      Just (name, Y) -> return (name, fl)
      Just (name, LY) -> return (name, deLitify fl)

  -- Parse, using (bootstrapping) parser.

  abssyn <- case parseYFileContents file of
      Left err -> die (fl_name ++ ':' : err)
      Right abssyn -> return abssyn

  -- Mangle the syntax into something useful.

  g <- case {-# SCC "Mangler" #-} mangler fl_name abssyn of
      Left  s -> die (unlines s ++ "\n")
      Right g -> return g

  optPrint dumpMangle $ putStr $ show g

  -- Pretty print the AbsSyn.

  case prettyFilename of
    Just s -> do
      let out = render (ppAbsSyn abssyn)
      writeFile s out
      hPutStrLn stderr ("Production rules written to: " ++ s)
    Nothing -> return ()

  return g

  where
    dumpMangle = DumpMangle `elem` flags
    optPrint b io = when b (putStr "\n---------------------\n" >> io)

getPrettyFileName :: String -> [Flag] -> IO (Maybe String)
getPrettyFileName baseName cli = case [ s | (OptPrettyFile s) <- cli ] of
  []        -> return Nothing
  [f] -> case f of
    Nothing -> return (Just (baseName ++ ".grammar"))
    Just j  -> return (Just j)
  _many     -> dieHappy "multiple --pretty/-p options\n"

-- Get filename without extension – exit if extension is neither .y nor .ly
getBaseName :: String -> IO String
getBaseName = getBaseName' . reverse where
  getBaseName' ('y':'l':'.':nm) = return (reverse nm)
  getBaseName' ('y':'.':nm)     = return (reverse nm)
  getBaseName' f                = dieHappy ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)
  
dieHappy :: String -> IO a
dieHappy s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
  where str' `withoutSuffix` suff
          | suff `isSuffixOf` str' = take (length str' - length suff) str'
          | otherwise              = str'