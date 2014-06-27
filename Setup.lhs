#!/usr/bin/env runhaskell

\begin{code}
{-# OPTIONS -fwarn-unused-imports #-}
module Main where

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.Setup (BuildFlags(..), buildVerbosity, fromFlagOrDefault)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program
import Distribution.Simple.Utils (notice)
import Distribution.Verbosity (normal)

import Control.Exception (try)
import Control.Monad (unless, when)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import System.Directory (removeFile, createDirectoryIfMissing, copyFile)
import System.FilePath (splitFileName, (</>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    buildHook      = myBuild
  , postBuild      = myPostBuild
  , postClean      = myPostClean
  , copyHook       = myCopy
  , instHook       = myInstall
  }

-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into
-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
mungeLinePragma line = case symbols line of
 syms | Just prag <- getLinePrag syms  -> prag
 -- Also convert old-style CVS lines, no idea why we do this...
 ("--":"$":"Id":":":_) -> filter (/='$') line
 (     "$":"Id":":":_) -> filter (/='$') line
 _ -> line

getLinePrag :: [String] -> Maybe String
getLinePrag ("#" : n : string : rest)
  | length rest <= 1   -- clang puts an extra field
  , length string >= 2 && head string == '"' && last string == '"'
  , all isDigit n
  = Just $ "{-# LINE " ++ n ++ " " ++ string ++ " #-}"
getLinePrag other = Nothing

symbols :: String -> [String]
symbols cs = case lex cs of
              (sym, cs'):_ | not (null sym) -> sym : symbols cs'
              _ -> []

{-|
  Since Happy technically depends on itself (it has .ly source files
  that need to be parsed in order to compile it), we ship pre-parsed
  versions of those files in the case that a Happy executable is not
  already available for use.  This hook checks whether we are in the
  position of requiring these saved build artifacts or being able to
  produce them (which are mutually exclusive scenarios) and performs
  the corresponding action before, resp. after, the build procedure.
 -}
shippedArtifacts :: [FilePath]
shippedArtifacts =
  [ "happy" </> "happy-tmp" </> "AttrGrammarParser.hs"
  , "happy" </> "happy-tmp" </> "Parser.hs"
  ]

copyArtifact :: FilePath -> FilePath -> FilePath -> IO ()
copyArtifact src dst f = do
  createDirectoryIfMissing True $ dst </> (fst $ splitFileName f)
  copyFile (src </> f) (dst </> f)

myBuild pkg_descr lbi hooks flags = do
  let verbosity = fromFlagOrDefault normal $ buildVerbosity flags
  happyExistence <- programFindLocation happyProgram verbosity
                    (getProgramSearchPath (withPrograms lbi))

  unless (isJust happyExistence) $ do
    -- life is not all sunshine and roses
    notice verbosity "No existing happy parser; using saved artifacts..."
    mapM_ (copyArtifact "artifacts" (buildDir lbi)) shippedArtifacts

  buildHook simpleUserHooks pkg_descr lbi hooks flags

  when (isJust happyExistence) $ do
    -- life is all sunshine and roses
    notice verbosity "Saving build artifacts for future generations..."
    mapM_ (copyArtifact (buildDir lbi) "artifacts") shippedArtifacts

myPostBuild _ flags _ lbi = do
  let runProgram p = rawSystemProgramConf (fromFlagOrDefault normal (buildVerbosity flags))
                                          p
                                          (withPrograms lbi)
      cpp_template src dst opts = do
        let tmp = dst ++ ".tmp"
        runProgram ghcProgram (["-o", tmp, "-E", "-cpp", "templates" </> src] ++ opts)
        writeFile dst . unlines . map mungeLinePragma . lines =<< readFile tmp
        removeFile tmp

  sequence_ ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
             [ cpp_template "GLR_Base.hs"       dst opts | (dst,opts) <- glr_base_templates ] ++
             [ cpp_template "GLR_Lib.hs"        dst opts | (dst,opts) <- glr_templates ])

myPostClean _ _ _ _ = mapM_ (try' . removeFile) all_template_files
  where try' :: IO a -> IO (Either IOError a)
        try' = try

myInstall pkg_descr lbi hooks flags =
  instHook simpleUserHooks pkg_descr' lbi hooks flags
  where pkg_descr' = pkg_descr {
          dataFiles = dataFiles pkg_descr ++ all_template_files
	}

myCopy pkg_descr lbi hooks copy_flags =
  copyHook simpleUserHooks pkg_descr' lbi hooks copy_flags
  where pkg_descr' = pkg_descr {
          dataFiles = dataFiles pkg_descr ++ all_template_files
	}

all_template_files :: [FilePath]
all_template_files = map fst (templates ++ glr_base_templates ++ glr_templates)

templates :: [(FilePath,[String])]
templates = [
  ("HappyTemplate"			, []),
  ("HappyTemplate-ghc"			, ["-DHAPPY_GHC"]),
  ("HappyTemplate-coerce"		, ["-DHAPPY_GHC","-DHAPPY_COERCE"]),
  ("HappyTemplate-arrays"		, ["-DHAPPY_ARRAY"]),
  ("HappyTemplate-arrays-ghc"		, ["-DHAPPY_ARRAY","-DHAPPY_GHC"]),
  ("HappyTemplate-arrays-coerce"	, ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_COERCE"]),
  ("HappyTemplate-arrays-debug"		, ["-DHAPPY_ARRAY","-DHAPPY_DEBUG"]),
  ("HappyTemplate-arrays-ghc-debug"	, ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),
  ("HappyTemplate-arrays-coerce-debug"	, ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_COERCE","-DHAPPY_DEBUG"])
 ]

glr_base_templates :: [(FilePath,[String])]
glr_base_templates = [
  ("GLR_Base"		, [])
 ]

glr_templates :: [(FilePath,[String])]
glr_templates = [
  ("GLR_Lib"		, []),
  ("GLR_Lib-ghc"	, ["-DHAPPY_GHC"]),
  ("GLR_Lib-ghc-debug"	, ["-DHAPPY_GHC", "-DHAPPY_DEBUG"])
 ]

\end{code}
