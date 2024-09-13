module Happy.Backend.LALR where

import Paths_happy_lib
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

importsToInject :: Bool -> String
importsToInject debug = concat ["\n", import_array, import_bits, import_glaexts, debug_imports, applicative_imports]
    where
      debug_imports  | debug     = import_debug
                     | otherwise = ""
      applicative_imports        = import_applicative

      import_glaexts     = "import qualified GHC.Exts as Happy_GHC_Exts\n"
      import_array       = "import qualified Data.Array as Happy_Data_Array\n"
      import_bits        = "import qualified Data.Bits as Bits\n"
      import_debug       = "import qualified System.IO as Happy_System_IO\n" ++
                           "import qualified System.IO.Unsafe as Happy_System_IO_Unsafe\n" ++
                           "import qualified Debug.Trace as Happy_Debug_Trace\n"
      import_applicative = "import Control.Applicative(Applicative(..))\n" ++
                           "import Control.Monad (ap)\n"

langExtsToInject :: [String]
langExtsToInject = ["MagicHash", "BangPatterns", "TypeSynonymInstances", "FlexibleInstances", "NoStrictData"]

defines :: Bool -> Bool -> String
defines debug coerce = unlines [ "#define " ++ d ++ " 1" | d <- vars_to_define ]
  where
  vars_to_define = concat
    [ [ "HAPPY_DEBUG"  | debug ]
    , [ "HAPPY_COERCE" | coerce ]
    ]
