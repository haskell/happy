module Main (main) where

import           Control.Monad
import           Language.Preprocessor.Cpphs
import           System.Directory
import           System.FilePath

main :: IO ()
main = do
  sequence_ ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
             [ cpp_template "GLR_Base.hs"        dst opts | (dst,opts) <- glr_base_templates ] ++
             [ cpp_template "GLR_Lib.hs"         dst opts | (dst,opts) <- glr_templates ])
 
  putStrLn ""
  putStrLn "-- fragment for happy.cabal file"
  putStrLn "data-dir: data/"
  putStrLn ""
  putStrLn "data-files:"
  forM_ all_template_files $ \fn -> putStrLn ("        " ++ fn)
  putStrLn "-- end of fragment"
  putStrLn ""
  putStrLn "You can invoke `cabal sdist` now" 

cpp_template :: FilePath -> FilePath -> [String] -> IO ()
cpp_template src0 dst0 defs = do
    ex <- doesFileExist src
    unless ex $
      fail ("file " ++ show src ++ " not found; are you in the right directory?")

    putStrLn ("generating " ++ show dst ++ "  (from " ++ show src ++ ")...")
    createDirectoryIfMissing False "data"
    srcdat <- readFile src
    outdat <- runCpphs cppflags src =<< readFile src
    writeFile dst outdat

    return ()
  where
    src = "templates" </> src0
    dst = "data"      </> dst0

    cppflags = defaultCpphsOptions
      { defines = [(d,"1") | d <- defs ]
      , boolopts = defaultBoolOptions
                   { hashline  = False
                   , locations = True
                   , ansi      = False
                   , macros    = True
                   }
      }


all_template_files :: [FilePath]
all_template_files = map fst (templates ++ glr_base_templates ++ glr_templates)

templates :: [(FilePath,[String])]
templates = [
  ("HappyTemplate"                      , []),
  ("HappyTemplate-ghc"                  , ["HAPPY_GHC"]),
  ("HappyTemplate-coerce"               , ["HAPPY_GHC","HAPPY_COERCE"]),
  ("HappyTemplate-arrays"               , ["HAPPY_ARRAY"]),
  ("HappyTemplate-arrays-ghc"           , ["HAPPY_ARRAY","HAPPY_GHC"]),
  ("HappyTemplate-arrays-coerce"        , ["HAPPY_ARRAY","HAPPY_GHC","HAPPY_COERCE"]),
  ("HappyTemplate-arrays-debug"         , ["HAPPY_ARRAY","HAPPY_DEBUG"]),
  ("HappyTemplate-arrays-ghc-debug"     , ["HAPPY_ARRAY","HAPPY_GHC","HAPPY_DEBUG"]),
  ("HappyTemplate-arrays-coerce-debug"  , ["HAPPY_ARRAY","HAPPY_GHC","HAPPY_COERCE","HAPPY_DEBUG"])
 ]

glr_base_templates :: [(FilePath,[String])]
glr_base_templates = [
  ("GLR_Base"           , [])
 ]

glr_templates :: [(FilePath,[String])]
glr_templates = [
  ("GLR_Lib"            , []),
  ("GLR_Lib-ghc"        , ["HAPPY_GHC"]),
  ("GLR_Lib-ghc-debug"  , ["HAPPY_GHC", "HAPPY_DEBUG"])
 ]
