module Happy.CLI.Dying (die, dieUsage, dieHappy, bye, byeUsage, getProgramName, usageHeader) where

import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (liftM)
import Data.List (isSuffixOf)

#if MIN_VERSION_base(4,8,0)
import System.Exit (die)
#else
import System.IO
die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1) 
#endif

dieHappy :: String -> IO a
dieHappy s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

byeUsage :: [OptDescr a] -> String -> IO b
byeUsage opts s = getProgramName >>= bye . (s ++) . usageHeader opts

dieUsage :: [OptDescr a] -> String -> IO b
dieUsage opts s = getProgramName >>= die . (s ++) . usageHeader opts

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str' `withoutSuffix` suff
            | suff `isSuffixOf` str' = take (length str' - length suff) str'
            | otherwise              = str'

usageHeader :: [OptDescr a] -> String -> String
usageHeader opts prog = usageInfo header opts where
  header = "Usage: " ++ prog ++ " [OPTION...] file\n"