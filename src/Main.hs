module Main where

import qualified FrontendCLI
import qualified MiddleendCLI
import qualified BackendCLI
import System.Environment
import OptionParsing
import GenUtils
import Control.Monad.Except
import System.Console.GetOpt
import Paths_happy (version)

-- Flag conglomerate
data HappyFlag = Frontend FrontendCLI.Flag | Middleend MiddleendCLI.Flag | Backend BackendCLI.Flag deriving Eq

as :: Functor f => [f a] -> (a -> b) -> [f b]
a `as` b = map (fmap b) a

allOptions :: [OptDescr HappyFlag]
allOptions = FrontendCLI.options `as` Frontend  ++ MiddleendCLI.options `as` Middleend ++ BackendCLI.options `as` Backend

getFrontend :: [HappyFlag] -> [FrontendCLI.Flag]
getMiddleend :: [HappyFlag] -> [MiddleendCLI.Flag]
getBackend :: [HappyFlag] -> [BackendCLI.Flag]
getFrontend flags = [a | Frontend a <- flags]
getMiddleend flags = [a | Middleend a <- flags]
getBackend flags = [a | Backend a <- flags]

-- Main
main :: IO ()
main = do
  let sortedOpts = beginOptionsWith "oip" allOptions -- outfile, info, pretty
  (flags, freeOpts) <- parseOptions sortedOpts version =<< getArgs
  filename <- requireUnnamedArgument freeOpts sortedOpts DieUsage0 DieUsageMult

  basename <- FrontendCLI.getBaseName filename
  grammar <- try $ FrontendCLI.parseAndRun (getFrontend flags) filename basename

  (action, goto, _, _) <- MiddleendCLI.parseAndRun (getMiddleend flags) filename basename grammar

  BackendCLI.parseAndRun (getBackend flags) basename grammar action goto

try :: IO (Either String a) -> IO a
try f = do
  result <- f
  case result of
    Left err -> liftIO $ die err
    Right a -> return a