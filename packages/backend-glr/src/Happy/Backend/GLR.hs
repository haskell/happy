module Happy.Backend.GLR(GLRBackendArgs(..), runGLRBackend) where

import Prelude hiding (filter)
import Happy.Backend.GLR.ProduceCode
import Happy.Grammar.Grammar
import Happy.Tabular.Tables
import Paths_happy_backend_glr
import Data.Maybe

-------- Main entry point (runGLRBackend) --------

data GLRBackendArgs = GLRBackendArgs {
  outFile :: String,
  templateDir :: Maybe String,
  decode :: Bool,
  filter :: Bool,
  ghc :: Bool,
  debug :: Bool
}

runGLRBackend :: GLRBackendArgs -> Grammar -> ActionTable -> GotoTable -> IO ()
runGLRBackend args g action goto = do
    defaultDir <- getDataDir
    let header = fromMaybe "" (hd g) ++ importsToInject args
    let templateDir' = fromMaybe defaultDir (templateDir args)
    produceCode args g action goto header templateDir'

-------- Helpers --------

produceCode :: GLRBackendArgs -> Grammar -> ActionTable -> GotoTable -> String -> String -> IO ()
produceCode args g action goto header template_dir = do
    let glr_decode = if decode args then TreeDecode else LabelDecode
        filtering = if filter args then UseFiltering else NoFiltering
        ghc_exts = if ghc args then UseGhcExts (importsToInject args) (langExtsToInject args) else NoGhcExts -- Don't always pass CPP, because only one of the files needs it.
    produceGLRParser (outFile args) template_dir action goto (Just header) (tl g) (debug args, (glr_decode, filtering, ghc_exts)) g

importsToInject :: GLRBackendArgs -> String
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

langExtsToInject :: GLRBackendArgs -> [String]
langExtsToInject args
  | ghc args = ["MagicHash", "BangPatterns", "TypeSynonymInstances", "FlexibleInstances"]
  | otherwise                = []