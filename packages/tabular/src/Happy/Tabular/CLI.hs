module Happy.Tabular.CLI(Flag(..), options, parseFlags, parseAndRun) where

import Happy.CLI.Dying
import Happy.Tabular
import Happy.Grammar.Grammar
import System.Console.GetOpt

-------- CLI flags and options --------

data Flag =
    OptInfoFile (Maybe String) |
    DumpLR0 |
    DumpLA |
    DumpAction |
    DumpGoto
  deriving Eq

options :: [OptDescr Flag]
options = [
    Option "i" ["info"] (OptArg OptInfoFile "FILE") "put detailed grammar info in FILE"
  
#ifdef DEBUG
  , Option "" ["lr0"] (NoArg DumpLR0) "Dump LR0 item sets",
    Option "" ["action"] (NoArg DumpAction) "Dump action table",
    Option "" ["goto"] (NoArg DumpGoto) "Dump goto table",
    Option "" ["lookaheads"] (NoArg DumpLA) "Dump lookahead info"
#endif

  ]

-------- [Flag] to TabularArgs conversion --------

parseAndRun :: [Flag] -> String -> String -> Grammar -> IO TabularResult
parseAndRun flags filename basename grammar = (parseFlags flags filename basename) >>= flip runTabular grammar

parseFlags :: [Flag] -> String -> String -> IO TabularArgs
parseFlags cli fileName baseName = do
  infoFile' <- getInfoFileName baseName cli
  return TabularArgs {
    inFile = fileName,
    infoFile = infoFile',
    dumpLR0 = DumpLR0 `elem` cli,
    dumpLA = DumpLA `elem` cli,
    dumpAction = DumpAction `elem` cli,
    dumpGoto = DumpGoto `elem` cli
  }

getInfoFileName :: String -> [Flag] -> IO (Maybe String)
getInfoFileName base cli = case [ s | (OptInfoFile s) <- cli ] of
  []        -> return Nothing
  [f] -> case f of
    Nothing -> return (Just (base ++ ".info"))
    Just j  -> return (Just j)
  _many     -> dieHappy "multiple --info/-i options\n"