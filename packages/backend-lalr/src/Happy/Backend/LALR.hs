module Happy.Backend.LALR where

import Paths_happy_backend_lalr
import Data.Char

lalrBackendDataDir :: IO String
lalrBackendDataDir = getDataDir

magicFilter :: Maybe String -> String -> String
magicFilter magicName = case magicName of
    Nothing -> id
    Just name' -> let
        small_name = name'
        big_name = toUpper (head name') : tail name'
        filter_output ('h':'a':'p':'p':'y':rest) = small_name ++ filter_output rest
        filter_output ('H':'a':'p':'p':'y':rest) = big_name ++ filter_output rest
        filter_output (c:cs) = c : filter_output cs
        filter_output [] = []
      in filter_output

importsToInject :: Bool -> Bool -> String
importsToInject ghc debug = concat ["\n", import_array, import_list, import_bits, glaexts_import, debug_imports, applicative_imports]
    where
      glaexts_import | ghc       = import_glaexts ++ import_ghcstack
                     | otherwise = ""
      debug_imports  | debug     = import_debug
                     | otherwise = ""
      applicative_imports        = import_applicative

      import_glaexts     = "import qualified GHC.Exts as Happy_GHC_Exts\n"
      import_ghcstack    = "import qualified GHC.Stack as Happy_GHC_Stack\n"
      import_array       = "import qualified Data.Array as Happy_Data_Array\n"
      import_list        = "import qualified Data.List as Happy_Data_List\n"
      import_bits        = "import qualified Data.Bits as Bits\n"
      import_debug       = "import qualified System.IO as Happy_System_IO\n" ++
                           "import qualified System.IO.Unsafe as Happy_System_IO_Unsafe\n" ++
                           "import qualified Debug.Trace as Happy_Debug_Trace\n"
      import_applicative = "import Control.Applicative(Applicative(..))\n" ++
                           "import Control.Monad (ap)\n"

langExtsToInject :: Bool -> [String]
langExtsToInject ghc
  | ghc = ["MagicHash", "BangPatterns", "TypeSynonymInstances", "FlexibleInstances"]
  | otherwise = []

defines :: Bool -> Bool -> Bool -> Bool -> String
defines debug array ghc coerce = unlines [ "#define " ++ d ++ " 1" | d <- vars_to_define ]
  where
  vars_to_define = concat
    [ [ "HAPPY_DEBUG"  | debug ]
    , [ "HAPPY_ARRAY"  | array ]
    , [ "HAPPY_GHC"    | ghc ]
    , [ "HAPPY_COERCE" | coerce ]
    ]
