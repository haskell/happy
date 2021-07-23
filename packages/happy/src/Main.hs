{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import qualified Happy.Frontend.CLI as FrontendCLI
import qualified Happy.Middleend.CLI as MiddleendCLI
import qualified Happy.Backend.CLI as BackendCLI
import qualified Happy.Backend.GLR.CLI as GLRBackendCLI
import qualified Happy.Backend as Backend
import qualified Happy.Backend.GLR as GLRBackend
import Happy.Core.OptionParsing
import Happy.Core.GenUtils
import Control.Monad.Except
import System.Console.GetOpt
import System.Environment
import Paths_happy (version)

-- All flags
data HappyFlag = Frontend FrontendCLI.Flag | Middleend MiddleendCLI.Flag | Backend BackendCLI.Flag | GLRBackend GLRBackendCLI.Flag deriving Eq

as :: Functor f => [f a] -> (a -> b) -> [f b]
a `as` b = map (fmap b) a

getFrontend :: [HappyFlag] -> [FrontendCLI.Flag]
getMiddleend :: [HappyFlag] -> [MiddleendCLI.Flag]
getBackend :: [HappyFlag] -> [BackendCLI.Flag]
getGLRBackend :: [HappyFlag] -> [GLRBackendCLI.Flag]
getFrontend flags = [a | Frontend a <- flags]
getMiddleend flags = [a | Middleend a <- flags]
getBackend flags = [a | Backend a <- flags]
getGLRBackend flags = [a | GLRBackend a <- flags]

-- Stick options togehter from all packages
allOptions :: [OptDescr HappyFlag]
allOptions =
  FrontendCLI.options `as` Frontend ++
  MiddleendCLI.options `as` Middleend ++
  BackendCLI.options `as` Backend ++
  -- Add the "--glr" option. Remove options that are already declared in happy-backend like outfile, template, ghc, debug.
  removeAllOverlaps BackendCLI.options (GLRBackendCLI.characteristicOption : GLRBackendCLI.options) `as` GLRBackend

-- Main
main :: IO ()
main = do
  let sortedOpts = beginOptionsWith "oip" allOptions -- Order: outfile, info, pretty
  (flags, freeOpts) <- parseOptions sortedOpts version =<< getArgs
  filename <- requireUnnamedArgument freeOpts sortedOpts DieUsage0 DieUsageMult
  basename <- FrontendCLI.getBaseName filename

  grammar <- try $ FrontendCLI.parseAndRun (getFrontend flags) filename basename
  (action, goto, _, _) <- MiddleendCLI.parseAndRun (getMiddleend flags) filename basename grammar

  -- Backend / GLRBackend switching
  let backendFlags = getBackend flags
  let glrFlags = getGLRBackend flags
  backendOpts <- BackendCLI.parseFlags backendFlags basename
  if GLRBackendCLI.hasCharacteristicFlag glrFlags
  then do
    -- Fill those glr-options that were removed due to overlap with happy-backend's options
    let glrOpts' = GLRBackendCLI.parseFlags glrFlags basename
    let glrOpts = glrOpts' {
      GLRBackend.outFile = Backend.outFile backendOpts,
      GLRBackend.templateDir = Backend.templateDir backendOpts,
      GLRBackend.ghc = Backend.ghc backendOpts,
      GLRBackend.debug = Backend.debug backendOpts
      }
    GLRBackend.runGLRBackend glrOpts grammar action goto
  else do
    Backend.runBackend backendOpts grammar action goto

try :: IO (Either String a) -> IO a
try f = do
  result <- f
  case result of
    Left err -> liftIO $ die err
    Right a -> return a