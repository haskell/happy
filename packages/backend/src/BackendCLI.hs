module BackendCLI(runBackend, Target(..), BackendOpts(..)) where

import Frontend
import Middleend
import Target
import ProduceCode
import ProduceGLRCode
import Paths_backend
import Data.Char
import Data.Maybe

data BackendOpts = BackendOpts {
  outFile :: String,
  templateDir :: Maybe String,
  magicName :: Maybe String,
  strict :: Bool,
  ghc :: Bool,
  coerce :: Bool, -- requires ghc
  target :: Target,
  debug :: Bool, -- requires target = TargetArrayBased
  glr :: Bool,
  glrDecode :: Bool, -- requires glr
  glrFilter :: Bool -- requires glr
}

runBackend :: BackendOpts -> Grammar -> ActionTable -> GotoTable -> IO ()
runBackend opts g action goto = do
    defaultDir <- getDataDir
    let header = (case hd g of Just s -> s; Nothing -> "") ++ importsToInject opts
        templateDir' = fromMaybe defaultDir (templateDir opts)
        produce = if glr opts then produceGLRCode else produceCode
    produce opts g action goto header templateDir'

produceCode :: BackendOpts -> Grammar -> ActionTable -> GotoTable -> String -> String -> IO ()
produceCode opts g action goto header templateDir = do
    template <- readFile (templateDir ++ "/HappyTemplate.hs")
    let outfile = produceParser g action goto ("CPP" : langExtsToInject opts) -- CPP is needed in all cases with unified template
                                (Just header) (tl g) (target opts) (coerce opts) (ghc opts) (strict opts)
        write = (if outFile opts == "-" then putStr else writeFile $ outFile opts)
    write $ magicFilter opts (outfile ++ defines opts ++ template)

produceGLRCode :: BackendOpts -> Grammar -> ActionTable -> GotoTable -> String -> String -> IO ()
produceGLRCode opts g action goto header templateDir =
    let glr_decode = if glrDecode opts then TreeDecode else LabelDecode
        filtering = if glrFilter opts then UseFiltering else NoFiltering
        ghc_exts = if ghc opts then UseGhcExts (importsToInject opts) (langExtsToInject opts) else NoGhcExts -- For GLR, don't always pass CPP, because only one of the files needs it.
    in produceGLRParser (outFile opts) templateDir action goto (Just header) (tl g) (debug opts, (glr_decode, filtering, ghc_exts)) g

magicFilter :: BackendOpts -> String -> String
magicFilter opts = case magicName opts of
    Nothing -> id
    Just name' -> let
        small_name = name'
        big_name = toUpper (head name') : tail name'
        filter_output ('h':'a':'p':'p':'y':rest) = small_name ++ filter_output rest
        filter_output ('H':'a':'p':'p':'y':rest) = big_name ++ filter_output rest
        filter_output (c:cs) = c : filter_output cs
        filter_output [] = []
      in filter_output

importsToInject :: BackendOpts -> String
importsToInject opts = concat ["\n", import_array, import_bits, glaexts_import, debug_imports, applicative_imports]
    where
      glaexts_import | ghc opts       = import_glaexts
                     | otherwise      = ""
      debug_imports  | debug opts     = import_debug
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

langExtsToInject :: BackendOpts -> [String]
langExtsToInject opts
  | ghc opts = ["MagicHash", "BangPatterns", "TypeSynonymInstances", "FlexibleInstances"]
  | otherwise                = []

defines :: BackendOpts -> String
defines opts = unlines [ "#define " ++ d ++ " 1" | d <- vars_to_define ]
  where
  vars_to_define = concat
    [ [ "HAPPY_DEBUG"  | debug opts ]
    , [ "HAPPY_ARRAY"  | target opts == TargetArrayBased ]
    , [ "HAPPY_GHC"    | ghc opts ]
    , [ "HAPPY_COERCE" | coerce opts ]
    ]