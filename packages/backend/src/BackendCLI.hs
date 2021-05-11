module BackendCLI(Flag(..), options, parseFlags, parseAndRun) where

import System.Console.GetOpt
import Grammar
import Tables
import Backend
import GenUtils
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
    OptDebugParser |
    OptGLR |
    OptGLR_Decode |
    OptGLR_Filter
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
    Option "d" ["debug"] (NoArg OptDebugParser) "produce a debugging parser (only with -a)",
    Option "l" ["glr"] (NoArg OptGLR) "Generate a GLR parser for ambiguous grammars",
    Option "k" ["decode"] (NoArg OptGLR_Decode) "Generate simple decoding code for GLR result",
    Option "f" ["filter"] (NoArg OptGLR_Filter) "Filter the GLR parse forest with respect to semantic usage"
  ]

-------- [Flag] to BackendArgs conversion --------

parseAndRun :: [Flag] -> String -> Grammar -> ActionTable -> GotoTable -> IO ()
parseAndRun flags basename grammar action goto = (parseFlags flags basename) >>= (\args -> runBackend args grammar action goto)

parseFlags :: [Flag] -> String -> IO BackendArgs
parseFlags cli baseName = do
  out_file <- getOutputFileName baseName cli
  template <- getTemplate cli
  magic_name <- getMagicName cli
  target' <- getTarget cli
  coerce' <- getCoerce cli
  debug' <- getDebug cli
  return BackendArgs {
    outFile = out_file,
    templateDir = template,
    magicName = magic_name,
    strict = getStrict cli,
    ghc = getGhc cli,
    coerce = coerce',
    target = target',
    debug = debug',
    glr = getGLR cli,
    glrDecode = getGLRDecode cli,
    glrFilter = getGLRFilter cli
  }

getTarget :: [Flag] -> IO Target
getTarget cli = case [ t | (Just t) <- map optToTarget cli ] of
  (t:ts) | all (==t) ts -> return t
  []                    -> return TargetHaskell
  _                     -> dieHappy "multiple target options\n"

optToTarget :: Flag -> Maybe Target
optToTarget OptArrayTarget    = Just TargetArrayBased
optToTarget _                 = Nothing

getOutputFileName :: String -> [Flag] -> IO String
getOutputFileName base cli = case [ s | (OptOutputFile s) <- cli ] of
  []    -> return (base ++ ".hs")
  list  -> return (last list)

getTemplate :: [Flag] -> IO (Maybe String)
getTemplate cli = case [ s | (OptTemplate s) <- cli ] of
  []    -> return Nothing
  list  -> return $ Just (last list)

getMagicName :: [Flag] -> IO (Maybe String)
getMagicName cli = case [ s | (OptMagicName s) <- cli ] of
  []    -> return Nothing
  list  -> return (Just (map toLower (last list)))

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

getGhc :: [Flag] -> Bool
getGhc = elem OptGhcTarget

getStrict :: [Flag] -> Bool
getStrict = elem OptStrict

getGLR :: [Flag] -> Bool
getGLR = elem OptGLR

getGLRFilter :: [Flag] -> Bool
getGLRFilter = elem OptGLR_Filter

getGLRDecode :: [Flag] -> Bool
getGLRDecode = elem OptGLR_Decode