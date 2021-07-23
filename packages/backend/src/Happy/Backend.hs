module Happy.Backend(BackendArgs(..), Target(..), runBackend) where

import Happy.Backend.Target
import Happy.Backend.ProduceCode
import Happy.Core.Grammar
import Happy.Core.Tables
import Paths_happy_backend
import Data.Char
import Data.Maybe

-------- Main entry point (runBackend) --------

data BackendArgs = BackendArgs {
  outFile :: String,
  templateDir :: Maybe String,
  magicName :: Maybe String,
  strict :: Bool,
  ghc :: Bool,
  coerce :: Bool, -- requires ghc
  target :: Target,
  debug :: Bool -- requires target = TargetArrayBased
}

runBackend :: BackendArgs -> Grammar -> ActionTable -> GotoTable -> IO ()
runBackend args g action goto = do
    defaultDir <- getDataDir
    let header = fromMaybe "" (hd g) ++ importsToInject args
    let templateDir' = fromMaybe defaultDir (templateDir args)
    produceCode args g action goto header templateDir'

-------- Helpers --------

produceCode :: BackendArgs -> Grammar -> ActionTable -> GotoTable -> String -> String -> IO ()
produceCode args g action goto header template_dir = do
    template <- readFile (template_dir ++ "/HappyTemplate.hs")
    let outfile = produceParser g action goto ("CPP" : langExtsToInject args) -- CPP is needed in all cases with unified template
                                (Just header) (tl g) (target args) (coerce args) (ghc args) (strict args)
    let write = (if outFile args == "-" then putStr else writeFile $ outFile args)
    write $ magicFilter args (outfile ++ defines args ++ template)

magicFilter :: BackendArgs -> String -> String
magicFilter args = case magicName args of
    Nothing -> id
    Just name' -> let
        small_name = name'
        big_name = toUpper (head name') : tail name'
        filter_output ('h':'a':'p':'p':'y':rest) = small_name ++ filter_output rest
        filter_output ('H':'a':'p':'p':'y':rest) = big_name ++ filter_output rest
        filter_output (c:cs) = c : filter_output cs
        filter_output [] = []
      in filter_output

importsToInject :: BackendArgs -> String
importsToInject args = concat ["\n", import_array, import_bits, glaexts_import, debug_imports, applicative_imports]
    where
      glaexts_import | ghc args       = import_glaexts
                     | otherwise      = ""
      debug_imports  | debug args     = import_debug
                     | otherwise      = ""
      applicative_imports = import_applicative

      import_glaexts     = "import qualified GHC.Exts as Happy_GHC_Exts\n"
      import_array       = "import qualified Data.Array as Happy_Data_Array\n"
      import_bits        = "import qualified Data.Bits as Bits\n"
      import_debug       = "import qualified System.IO as Happy_System_IO\n" ++
                           "import qualified System.IO.Unsafe as Happy_System_IO_Unsafe\n" ++
                           "import qualified Debug.Trace as Happy_Debug_Trace\n"
      import_applicative = "import Control.Applicative(Applicative(..))\n" ++
                           "import Control.Monad (ap)\n"

langExtsToInject :: BackendArgs -> [String]
langExtsToInject args
  | ghc args = ["MagicHash", "BangPatterns", "TypeSynonymInstances", "FlexibleInstances"]
  | otherwise                = []

defines :: BackendArgs -> String
defines args = unlines [ "#define " ++ d ++ " 1" | d <- vars_to_define ]
  where
  vars_to_define = concat
    [ [ "HAPPY_DEBUG"  | debug args ]
    , [ "HAPPY_ARRAY"  | target args == TargetArrayBased ]
    , [ "HAPPY_GHC"    | ghc args ]
    , [ "HAPPY_COERCE" | coerce args ]
    ]