#!/usr/bin/runhaskell

\begin{code}
module Main where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Setup
import Distribution.Compiler
--import Distribution.Compat.FilePath
import Text.Printf
import System.Process
import System.Directory
import System.Exit
import Control.Exception

main :: IO ()
main = defaultMainWithHooks defaultUserHooks{ postBuild = myPostBuild,
					      postClean = myPostClean,
					      postCopy  = myPostCopy,
					      postInst  = myPostInstall }

myPostBuild args verb pkg_descr lbi =
  excursion "templates" $ do
  let cpp_template src dst opts = do
	let dst_pp = dst ++ ".hspp"
	    cmd = printf "%s -E -cpp -o %s %s %s" 
			(compilerPath (compiler lbi)) dst_pp src (unwords opts)
		-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
		-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
	    perl = printf "perl -pe \'s/^#\\s+(\\d+)\\s+(\"[^\"]*\")/{-# LINE \\1 \\2 #-}/g;s/\\$(Id:.*)\\$/\\1/g' < %s > %s" dst_pp dst
	do_cmd cmd `cmd_seq` do_cmd perl
  cmd_seqs ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
  	    [ cpp_template "GLR_Base.lhs"    dst opts | (dst,opts) <- glr_base_templates ] ++
  	    [ cpp_template "GLR_Lib.lhs"    dst opts | (dst,opts) <- glr_templates ])

myPostClean args verb pkg_descr lbi =
  excursion "templates" $ do
  sequence [ removeFile f >> removeFile (f ++ ".hspp") | (f,_) <- all_templates]
  return ExitSuccess

myPostInstall args flags pkg_descr lbi =
  install pkg_descr lbi NoCopyDest

myPostCopy args (mb_copyPrefix,verb) pkg_descr lbi = 
  install pkg_descr lbi mb_copyPrefix

install pkg_descr lbi mb_copyPrefix =
  excursion "templates" $ do
  let datadir = mkDataDir pkg_descr lbi mb_copyPrefix
  createDirectoryIfMissing True datadir
  sequence [ copyFile f (datadir ++ '/':f) | (f,_) <- all_templates ]  
	-- XXX: joinFileName
  return ExitSuccess

all_templates = templates ++ glr_base_templates ++ glr_templates

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

glr_base_templates = [
  ("GLR_Base"		, [])
 ]

glr_templates = [
  ("GLR_Lib"		, []),
  ("GLR_Lib-ghc"	, ["-DHAPPY_GHC"]),
  ("GLR_Lib-ghc-debug"	, ["-DHAPPY_GHC", "-DHAPPY_DEBUG"])
 ]

-- -----------------------------------------------------------------------------
-- Utils

do_cmd c = do
  putStrLn c
  runCommand c >>= waitForProcess

cmd_seq c1 c2 = do
  e <- c1
  case e of
	ExitSuccess -> c2
	_	    -> return e

cmd_seqs = foldr cmd_seq (return ExitSuccess)

excursion d io = do
  cwd <- getCurrentDirectory
  (do setCurrentDirectory d; io) `finally` setCurrentDirectory cwd
\end{code}
