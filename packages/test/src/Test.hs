module Test(test, TestSetup(..), defaultTestFiles, bootstrapTestFiles, defaultArguments) where

import System.Process
import System.Exit
import Paths_happy_test

data TestSetup = TestSetup {
  happyExec :: String,      -- (path to the) happy exeuctable which shall be tested. "happy" should suffice here for cabal test
  defaultTests :: [String], -- standard tests from happy-test package that should be performed. these are in this package's data-dir
  customTests :: [String],  -- custom tests from the calling package that should be performed. these are in the calling package's data-dir
  customDataDir :: String,  -- data-dir of the calling package. all tests are compiled and executed in their respective directory.
  allArguments :: [String], -- all different testable argument combinations for happy, as strings
  stopOnFailure :: Bool     -- continue with remaining tests after an error has occurred?
}

test :: TestSetup -> IO a
test setup = do
  defaultDir <- getDataDir
  let infiles = zip (repeat defaultDir) (defaultTests setup) ++
                zip (repeat (customDataDir setup)) (customTests setup)                      -- (dir, file.ly)
  let inandout = map (\(dir, file) -> (dir, file, hs file)) infiles                         -- (dir, file.ly, file.hs)
  let tests = [(d, i, o, arg) | (d, i, o) <- inandout, arg <- allArguments setup]
  result <- testCons tests (happyExec setup) (stopOnFailure setup)
  if result then exitSuccess else exitFailure
  where
    hs = hs' . reverse
    hs' ('y':'l':'.':file) = reverse file ++ ".hs"
    hs' ('y':'.':file) = reverse file ++ ".hs"
    hs' file = error $ "Error: test file does not end in .y or .ly: " ++ file

testCons :: [(String, String, String, String)] -> String -> Bool -> IO Bool
testCons [] _ _ = return True
testCons ((dir, infile, outfile, args):rest) happy stopOnFail = do
  result <- runSingleTest happy args dir infile outfile
  if result then testCons rest happy stopOnFail else
    if stopOnFail
      then return False
      else do _ <- testCons rest happy stopOnFail; return False

defaultTestFiles :: [String]
defaultTestFiles = ["Test.ly", "TestMulti.ly", "TestPrecedence.ly", "bug001.ly", "monad001.y", "monad002.ly", "precedence001.ly",
                    "precedence002.y", "bogus-token.y", "bug002.y", "Partial.ly", "issue91.y", "issue93.y", "issue94.y", "issue95.y",
                    "test_rules.y", "monaderror.y", "monaderror-explist.y", "typeclass_monad001.y", "typeclass_monad002.ly",
                    "typeclass_monad_lexer.y", "rank2.y", "shift01.y"]

bootstrapTestFiles :: [String]
bootstrapTestFiles = ["AttrGrammar001.y", "AttrGrammar002.y"]

defaultArguments :: [String]
defaultArguments = map ("--strict " ++) ["", "-a", "-g", "-ag", "-gc", "-agc"]

runSingleTest :: String -> String -> String -> String -> String -> IO Bool
runSingleTest happy arguments dir inputFile outputFile = do
  cabalPutStrLn $ "\ncd '" ++ dir ++ "'"

  success1 <- execVerboseIn dir [happy, inputFile, arguments, "-o", outputFile]
  if not success1 then fail' >> return False else do
  
  success2 <- execVerboseIn dir ["runhaskell", outputFile]
  if not success2 then fail' >> return False else do

  rm
  return True
  where
    rm = (system $ "cd '" ++ dir ++ "'; rm " ++ outputFile) >> return ()
    fail' = (cabalPutStrLn $ "Test " ++ inputFile ++ " failed!") >> rm

    cabalPutStrLn :: String -> IO () -- cabal hack
    cabalPutStrLn a = (system $ "echo '" ++ a ++ "'") >> return ()

    execVerboseIn :: String -> [String] -> IO Bool
    execVerboseIn inDir args = do
      let cmd = unwords args
      cabalPutStrLn cmd
      exitCode <- system $ "cd '" ++ inDir ++ "'; " ++ cmd
      return $ exitCode == ExitSuccess