module Main where

import FrontendCLI
import MiddleendCLI
import BackendCLI
import System.IO
import System.Exit (exitWith, ExitCode(..))
import System.Environment
import OptionParsing
import Control.Monad.Except
import System.Console.GetOpt

extInfo :: [OptDescr Int]
extInfo = []

main :: IO ()
main = do
    options <- parseOptions extInfo =<< getArgs
    grammar <- try $ runFrontend (frontendOpts options)
    (action, goto, _, _) <- runMiddleend (middleendOpts options) grammar
    runBackend (backendOpts options) grammar action goto

try :: IO (Either String a) -> IO a
try f = do
    result <- f
    case result of
      Left err -> liftIO $ die err
      Right a -> return a
    where
        die s = hPutStr stderr s >> exitWith (ExitFailure 1)