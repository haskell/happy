#!/usr/bin/runhaskell

\begin{code}
module Main where

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.Setup ( BuildFlags(..) )
import Distribution.Simple ( defaultMainWithHooks, defaultUserHooks, UserHooks(..) )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Program

import System.FilePath ((</>))
import System.IO.Error ( try )
import System.Directory (removeFile)

main :: IO ()
main = defaultMainWithHooks defaultUserHooks{ hookedPrograms = [perlProgram],
					      postBuild = myPostBuild,
					      postClean = myPostClean,
					      copyHook  = myCopy,
					      instHook  = myInstall }

perlProgram = simpleProgram "perl"

-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
crazy_perl_regexp =
 "s/^#\\s+(\\d+)\\s+(\"[^\"]*\")/{-# LINE \\1 \\2 #-}/g;s/\\$(Id:.*)\\$/\\1/g"

myPostBuild _ flags _ lbi = do
  let runProgram p = rawSystemProgramConf (buildVerbose flags) p (withPrograms lbi)
      cpp_template src dst opts = do
        runProgram ghcProgram (["-o", dst, "-E", "-cpp", "templates" </> src] ++ opts)
	runProgram perlProgram ["-i.bak", "-pe", crazy_perl_regexp, dst]

  sequence_ ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
             [ cpp_template "GLR_Base.lhs"       dst opts | (dst,opts) <- glr_base_templates ] ++
             [ cpp_template "GLR_Lib.lhs"        dst opts | (dst,opts) <- glr_templates ])

myPostClean _ _ _ _ = mapM_ (try . removeFile) all_template_files

myInstall pkg_descr lbi hooks flags =
  instHook defaultUserHooks pkg_descr' lbi hooks flags
  where pkg_descr' = pkg_descr {
          dataFiles = dataFiles pkg_descr ++ all_template_files
	}

myCopy pkg_descr lbi hooks copy_flags =
  copyHook defaultUserHooks pkg_descr' lbi hooks copy_flags
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
