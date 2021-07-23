module Happy.Backend.GLR.CLI(Flag(..), options, characteristicOption, hasCharacteristicFlag, parseFlags) where

import Prelude hiding (filter)
import Happy.Backend.GLR
import System.Console.GetOpt

-------- CLI flags and options --------

data Flag =
    OptGLR |
    OptOutputFile String |
    OptTemplate String |
    OptDecode |
    OptFilter |
    OptGhcTarget |
    OptDebugParser
  deriving Eq

-- All options, without the characteristic "glr" option.
options :: [OptDescr Flag]
options = [
    Option "o" ["outfile"] (ReqArg OptOutputFile "FILE") "write the output to FILE (default: file.hs)",
    Option "t" ["template"] (ReqArg OptTemplate "DIR") "look in DIR for template files",
    Option "k" ["decode"] (NoArg OptDecode) "Generate simple decoding code for GLR result",
    Option "f" ["filter"] (NoArg OptFilter) "Filter the GLR parse forest with respect to semantic usage",
    Option "g" ["ghc"] (NoArg OptGhcTarget) "use GHC extensions",
    Option "d" ["debug"] (NoArg OptDebugParser) "produce a debugging parser"
  ]

-- Use this option when using this backend together with another backend.
characteristicOption :: OptDescr Flag
characteristicOption = Option "l" ["glr"] (NoArg OptGLR) "Generate a GLR parser for ambiguous grammars"

-------- [Flag] to BackendArgs conversion --------

-- Determines whether the characteristic "glr" flag is set.
hasCharacteristicFlag :: [Flag] -> Bool
hasCharacteristicFlag = elem OptGLR

parseFlags :: [Flag] -> String -> GLRBackendArgs
parseFlags cli baseName = GLRBackendArgs {
    outFile = getOutputFileName baseName cli,
    templateDir = getTemplate cli,
    decode = OptDecode `elem` cli,
    filter = OptFilter `elem` cli,
    ghc = OptGhcTarget `elem` cli,
    debug = OptDebugParser `elem` cli
  }

getOutputFileName :: String -> [Flag] -> String
getOutputFileName base cli = case [ s | (OptOutputFile s) <- cli ] of
  []    -> base ++ ".hs"
  list  -> last list

getTemplate :: [Flag] -> Maybe String
getTemplate cli = case [ s | (OptTemplate s) <- cli ] of
  []    -> Nothing
  list  -> Just $ last list