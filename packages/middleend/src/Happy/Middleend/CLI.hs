module Happy.Middleend.CLI(Flag(..), options, parseFlags, parseAndRun) where

import Happy.Middleend
import Happy.Core.Grammar
import Happy.Core.GenUtils
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

-------- [Flag] to MiddleendArgs conversion --------

parseAndRun :: [Flag] -> String -> String -> Grammar -> IO MiddleendResult
parseAndRun flags filename basename grammar = (parseFlags flags filename basename) >>= flip runMiddleend grammar

parseFlags :: [Flag] -> String -> String -> IO MiddleendArgs
parseFlags cli fileName baseName = do
  infoFile' <- getInfoFileName baseName cli
  return MiddleendArgs {
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