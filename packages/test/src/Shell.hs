module Shell where

import System.Directory
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

-- Run a shell command inside a given directory. Succeed when exit code = 0. Ignore the command's output.
runCmdIn :: FilePath -> [String] -> Bool -> Shell
runCmdIn dir args verbose = do
  when verbose (liftIO . putStr $ "Inside " ++ dir ++ ": ")
  liftIO $ setCurrentDirectory dir
  runCmd args verbose

runCmd' :: [String] -> Bool -> IO Bool
runCmd' args verbose = runShell (runCmd args verbose)

runCmdIn' :: String -> [String] -> Bool -> IO Bool
runCmdIn' dir args verbose = runShell (runCmdIn dir args verbose)

-- dropWhileEnd only exists since base-4.5.0.0, i.e. GHC 7.4.1
#if !MIN_VERSION_base(4,5,0)
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []
#endif