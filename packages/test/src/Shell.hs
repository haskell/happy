module Shell where

import Data.List
import System.Process
import System.Exit
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Applicative

type TypedShell a = MaybeT IO a
type Shell = TypedShell ()

-- Perform a sequence of Shell operations, stopping when one fails.
runShell :: Shell -> IO Bool
runShell s = fmap (Nothing /=) (runMaybeT s)

-- Perform a; if a fails, perform b as cleanup, but still fail the operation.
(|||) :: TypedShell a -> IO b -> TypedShell a
a ||| b = MaybeT { runMaybeT = run }
  where run = do
          result <- runMaybeT a
          case result of
            Just val -> return $ Just val
            Nothing -> b >>= (\_ -> return Nothing)
infixr 2 |||

-- Run a shell command. Succeed when exit code = 0. Ignore the command's output.
runCmd :: [String] -> Bool -> Shell
runCmd args verbose = do
  let cmd = unwords args
  when verbose (liftIO $ putStrLn cmd)
  exitCode <- liftIO $ system cmd
  if exitCode == ExitSuccess
    then return ()
    else empty

runCmdIn :: String -> [String] -> Bool -> Shell
runCmdIn dir args verbose = runCmd (("cd '" ++ dir ++ "'; "):args) verbose

runCmd' :: [String] -> Bool -> IO Bool
runCmd' = runShell .* runCmd

runCmdIn' :: String -> [String] -> Bool -> IO Bool
runCmdIn' = runShell ..* runCmdIn

concatPaths :: FilePath -> FilePath -> FilePath
concatPaths a b = dropWhileEnd (== sep) a ++ sep : dropWhile (== sep) b
  where sep = '/'

(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) = (.) . (.)

(..*) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(..*) = (.) . (.) . (.)