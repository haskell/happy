module Test where

import Data.List (intercalate)
import GHC.Conc (numCapabilities)
import System.Process (system, readProcess)
import System.Exit (exitWith)
import Paths_happy_test

test :: IO a
test = do
  dir <- getDataDir
  print dir
  let jFlag = "-j" ++ show numCapabilities
  let cmd = ["make", jFlag, "-k", "-C", dir, "clean", "all"]
  print (intercalate " " cmd)
  system "pwd" >>= print
  t <- readProcess "pwd" [] ""
  print t
  system (intercalate " " cmd) >>= exitWith
