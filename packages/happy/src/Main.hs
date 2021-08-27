module Main where

import qualified Happy.Frontend.CLI as FrontendCLI
import qualified Happy.Tabular.CLI as TabularCLI
import qualified Happy.Backend.CLI as BackendCLI
import qualified Happy.Backend.GLR.CLI as GLRBackendCLI
import qualified Happy.Backend as Backend
import qualified Happy.Backend.GLR as GLRBackend
import Happy.CLI.Dying
import Happy.CLI.OptionParsing
import Control.Monad.Except
import System.Console.GetOpt
import System.Environment
import Paths_happy (version)

-- Option for switching between backend and glr-backend
useGLROption :: OptDescr TopLevelFlag
useGLROption = Option "l" ["glr"] (NoArg OptGLR) "Generate a GLR parser for ambiguous grammars"
data TopLevelFlag = OptGLR deriving Eq

-- Combine the flags from all the packages
data HappyFlag = TopLevel TopLevelFlag | Frontend FrontendCLI.Flag | Tabular TabularCLI.Flag | Backend BackendCLI.Flag | GLRBackend GLRBackendCLI.Flag deriving Eq

as :: Functor f => [f a] -> (a -> b) -> [f b]
a `as` b = map (fmap b) a

getTopLevel :: [HappyFlag] -> [TopLevelFlag]
getFrontend :: [HappyFlag] -> [FrontendCLI.Flag]
getTabular :: [HappyFlag] -> [TabularCLI.Flag]
getBackend :: [HappyFlag] -> [BackendCLI.Flag]
getGLRBackend :: [HappyFlag] -> [GLRBackendCLI.Flag]
getTopLevel flags = [a | TopLevel a <- flags]
getFrontend flags = [a | Frontend a <- flags]
getTabular flags = [a | Tabular a <- flags]
getBackend flags = [a | Backend a <- flags]
getGLRBackend flags = [a | GLRBackend a <- flags]

-- Stick options togehter from all packages
allOptions :: [OptDescr HappyFlag]
allOptions =
  FrontendCLI.options `as` Frontend ++
  TabularCLI.options `as` Tabular ++
  BackendCLI.options `as` Backend ++
  -- Add the "--glr" option. Remove options that are already declared in happy-backend like outfile, template, ghc, debug.
  [useGLROption] `as` TopLevel ++
  removeAllOverlaps BackendCLI.options GLRBackendCLI.options `as` GLRBackend

-- Main
main :: IO ()
main = do
  let sortedOpts = beginOptionsWith "oip" allOptions -- Order: outfile, info, pretty
  (flags, freeOpts) <- parseOptions sortedOpts version =<< getArgs
  filename <- requireUnnamedArgument freeOpts sortedOpts DieUsage0 DieUsageMult
  basename <- FrontendCLI.getBaseName filename

  grammar <- try $ FrontendCLI.parseAndRun (getFrontend flags) filename basename
  (action, goto, _, _) <- TabularCLI.parseAndRun (getTabular flags) filename basename grammar

  -- Backend / GLRBackend switching
  let useGLR = OptGLR `elem` getTopLevel flags
  backendOpts <- BackendCLI.parseFlags (getBackend flags) basename

  case useGLR of
    True -> GLRBackend.runGLRBackend (createGLROpts (getGLRBackend flags) backendOpts basename) grammar action goto
    False -> Backend.runBackend backendOpts grammar action goto

-- Fill those glr-options that were removed due to overlap with happy-backend's options
createGLROpts :: [GLRBackendCLI.Flag] -> Backend.BackendArgs -> String -> GLRBackend.GLRBackendArgs
createGLROpts glrFlags backendOpts basename =
  let glrOpts' = GLRBackendCLI.parseFlags glrFlags basename
  in glrOpts' {
    GLRBackend.outFile = Backend.outFile backendOpts,
    GLRBackend.templateDir = Backend.templateDir backendOpts,
    GLRBackend.ghc = Backend.ghc backendOpts,
    GLRBackend.debug = Backend.debug backendOpts
    }

try :: IO (Either String a) -> IO a
try f = do
  result <- f
  case result of
    Left err -> liftIO $ die err
    Right a -> return a