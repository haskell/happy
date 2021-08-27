module Happy.CLI.Dying (die, dieUsage, dieHappy, bye, byeUsage, getProgramName, usageHeader) where

import System.Console.GetOpt
import System.Environment
import System.Exit (die, exitWith, ExitCode(..))
import Control.Monad (liftM)
import Data.List (isSuffixOf)

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