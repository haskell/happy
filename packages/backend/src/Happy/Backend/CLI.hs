module Happy.Backend.CLI(Flag(..), options, parseFlags, parseAndRun) where

import Happy.Backend
import Happy.Core.Grammar
import Happy.Core.Tables
import Happy.Core.GenUtils
import System.Console.GetOpt
import Data.Char

-------- CLI flags and options --------

data Flag =
    OptOutputFile String |
    OptTemplate String |
    OptMagicName String |
    OptStrict |
    OptGhcTarget |
    OptUseCoercions |
    OptArrayTarget |
    OptDebugParser
  deriving Eq

options :: [OptDescr Flag]
options = [
    Option "o" ["outfile"] (ReqArg OptOutputFile "FILE") "write the output to FILE (default: file.hs)",
    Option "t" ["template"] (ReqArg OptTemplate "DIR") "look in DIR for template files",
    Option "m" ["magic-name"] (ReqArg OptMagicName "NAME") "use NAME as the symbol prefix instead of \"happy\"",
    Option "s" ["strict"] (NoArg OptStrict) "evaluate semantic values strictly (experimental)",
    Option "g" ["ghc"] (NoArg OptGhcTarget) "use GHC extensions",
    Option "c" ["coerce"] (NoArg OptUseCoercions) "use type coercions (only available with -g)",
    Option "a" ["array"] (NoArg OptArrayTarget) "generate an array-based parser",
    Option "d" ["debug"] (NoArg OptDebugParser) "produce a debugging parser (only with -a)"
  ]

-------- [Flag] to BackendArgs conversion --------

parseAndRun :: [Flag] -> String -> Grammar -> ActionTable -> GotoTable -> IO ()
parseAndRun flags basename grammar action goto = (parseFlags flags basename) >>= (\args -> runBackend args grammar action goto)

parseFlags :: [Flag] -> String -> IO BackendArgs
parseFlags cli baseName = do 
  target' <- getTarget cli
  coerce' <- getCoerce cli
  debug' <- getDebug cli
  return BackendArgs {
    outFile = getOutputFileName baseName cli,
    templateDir = getTemplate cli,
    magicName = getMagicName cli,
    strict = OptStrict `elem` cli,
    ghc = OptGhcTarget `elem` cli,
    coerce = coerce',
    target = target',
    debug = debug'
  }

getTarget :: [Flag] -> IO Target
getTarget cli = case [ t | (Just t) <- map optToTarget cli ] of
  (t:ts) | all (==t) ts -> return t
  []                    -> return TargetHaskell
  _                     -> dieHappy "multiple target options\n"

optToTarget :: Flag -> Maybe Target
optToTarget OptArrayTarget    = Just TargetArrayBased
optToTarget _                 = Nothing

getOutputFileName :: String -> [Flag] -> String
getOutputFileName base cli = case [ s | (OptOutputFile s) <- cli ] of
  []    -> base ++ ".hs"
  list  -> last list

getTemplate :: [Flag] -> Maybe String
getTemplate cli = case [ s | (OptTemplate s) <- cli ] of
  []    -> Nothing
  list  -> Just $ last list

getMagicName :: [Flag] -> Maybe String
getMagicName cli = case [ s | (OptMagicName s) <- cli ] of
  []    -> Nothing
  list  -> (Just (map toLower (last list)))

getCoerce :: [Flag] -> IO Bool
getCoerce cli
  | elem OptUseCoercions cli =
      if elem OptGhcTarget cli
        then return True
        else dieHappy "-c/--coerce may only be used in conjunction with -g/--ghc\n"
  | otherwise = return False

getDebug :: [Flag] -> IO Bool
getDebug cli
  | elem OptDebugParser cli =
      if elem OptArrayTarget cli
        then return True
        else dieHappy "-d/--debug may only be used in conjunction with -a/--array\n"
  | otherwise = return False