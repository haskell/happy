module Main where

import qualified FrontendCLI
import qualified MiddleendCLI
import qualified BackendCLI
import System.IO
import System.Exit (exitWith, ExitCode(..))
import System.Environment
import OptionParsing
import Control.Monad.Except
import System.Console.GetOpt

-- Flag conglomerate
data CustomFlag = Frontend FrontendCLI.Flag | Middleend MiddleendCLI.Flag | Backend BackendCLI.Flag deriving Eq

allOptions :: [OptDescr CustomFlag]
allOptions = map (fmap Frontend) FrontendCLI.options ++ map (fmap Middleend) MiddleendCLI.options ++ map (fmap Backend) BackendCLI.options

getFrontend :: [CustomFlag] -> [FrontendCLI.Flag]
getMiddleend :: [CustomFlag] -> [MiddleendCLI.Flag]
getBackend :: [CustomFlag] -> [BackendCLI.Flag]
getFrontend flags = [a | Frontend a <- flags ]
getMiddleend flags = [a | Middleend a <- flags ]
getBackend flags = [a | Backend a <- flags ]

-- Main
main :: IO ()
main = do
    (flags, filename) <- parseOptions allOptions =<< getArgs
    basename <- getBaseName filename
    grammar <- try $ FrontendCLI.parseAndRun (getFrontend flags) filename basename
    (action, goto, _, _) <- MiddleendCLI.parseAndRun (getMiddleend flags) filename basename grammar
    BackendCLI.parseAndRun (getBackend flags) basename grammar action goto

try :: IO (Either String a) -> IO a
try f = do
    result <- f
    case result of
      Left err -> liftIO $ die err
      Right a -> return a
    where
        die s = hPutStr stderr s >> exitWith (ExitFailure 1)