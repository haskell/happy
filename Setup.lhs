#!/usr/bin/runhaskell

\begin{code}
module Main where

import Control.Exception ( finally )
import Distribution.PackageDescription ( PackageDescription )
import Distribution.Setup ( BuildFlags, CleanFlags, CopyDest(..), CopyFlags(..), InstallFlags )
import Distribution.Simple ( defaultMainWithHooks, defaultUserHooks, UserHooks(..), Args, compilerPath )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), mkDataDir )
import System.Directory
import System.Exit ( ExitCode(..) )
import System.IO
import System.Process
import System.Cmd
import Text.Printf ( printf )

main :: IO ()
main = defaultMainWithHooks defaultUserHooks{ postBuild = myPostBuild,
					      postClean = myPostClean,
					      postCopy  = myPostCopy,
					      postInst  = myPostInstall }

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
myPostBuild _ _ _ lbi =
  excursion "templates" $ do
  let cpp_template src dst opts = do
	let dst_pp = dst ++ ".hspp"
	    ghc = compilerPath (compiler lbi)
	    ghc_args = ["-o", dst_pp, "-E", "-cpp", src] ++ opts
		-- hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
		-- '{-# LINE 27 "GenericTemplate.hs" #-}'.
	    perl_args = ["-pe", "s/^#\\s+(\\d+)\\s+(\"[^\"]*\")/{-# LINE \\1 \\2 #-}/g;s/\\$(Id:.*)\\$/\\1/g"]
	mb_perl <- findExecutable "perl"
	perl <- case mb_perl of
		  Nothing -> ioError (userError "You need \"perl\" installed and in your PATH to complete the build")
		  Just path -> return path
	do_cmd ghc ghc_args `cmd_seq` 
	   do h_dst_pp <- openFile dst_pp ReadMode
	      h_dst    <- openFile dst    WriteMode
	      putStrLn (perl ++ ' ':unwords perl_args)
	      p <- runProcess perl perl_args Nothing Nothing 
			(Just h_dst_pp) (Just h_dst) Nothing
	      waitForProcess p

  cmd_seqs ([ cpp_template "GenericTemplate.hs" dst opts | (dst,opts) <- templates ] ++
  	    [ cpp_template "GLR_Base.lhs"       dst opts | (dst,opts) <- glr_base_templates ] ++
  	    [ cpp_template "GLR_Lib.lhs"        dst opts | (dst,opts) <- glr_templates ])

myPostClean :: Args -> CleanFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO ExitCode
myPostClean _ _ _ _ =
  excursion "templates" $ do
  sequence [ removeFile f >> removeFile (f ++ ".hspp") | (f,_) <- all_templates]
  return ExitSuccess

myPostInstall :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
myPostInstall _ _ pkg_descr lbi =
  install pkg_descr lbi NoCopyDest

myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
myPostCopy _ copy_flags pkg_descr lbi = 
  install pkg_descr lbi (copyDest copy_flags)

install :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ExitCode
install pkg_descr lbi copy_dest =
  excursion "templates" $ do
  let dataDir = mkDataDir pkg_descr lbi copy_dest
  createDirectoryIfMissing True dataDir
  sequence [ copyFile f (dataDir ++ '/':f) | (f,_) <- all_templates ]  
	-- XXX: joinFileName
  return ExitSuccess

all_templates :: [(FilePath,[String])]
all_templates = templates ++ glr_base_templates ++ glr_templates

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

-- -----------------------------------------------------------------------------
-- Utils

do_cmd :: FilePath -> [String] -> IO ExitCode
do_cmd cmd args = do
  putStrLn (cmd ++ ' ':unwords args)
  rawSystem cmd args

cmd_seq :: IO ExitCode -> IO ExitCode -> IO ExitCode
cmd_seq c1 c2 = do
  e <- c1
  case e of
	ExitSuccess -> c2
	_	    -> return e

cmd_seqs :: [IO ExitCode] -> IO ExitCode
cmd_seqs = foldr cmd_seq (return ExitSuccess)

excursion :: FilePath -> IO a -> IO a
excursion d io = do
  cwd <- getCurrentDirectory
  (do setCurrentDirectory d; io) `finally` setCurrentDirectory cwd
\end{code}
