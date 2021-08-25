module Happy.Test(test, TestSetup(..), defaultTestFiles, attributeGrammarTestFiles, defaultArguments) where

import Happy.Test.Shell
import System.IO
import System.FilePath
import Control.Exception
import System.Directory
import System.Exit
import Paths_happy_test

data TestSetup = TestSetup {
  happyExec :: String,      -- name of the happy exeuctable which shall be tested.
  defaultTests :: [String], -- standard tests from happy-test package that should be performed. these are in this package's data-dir
  customTests :: [String],  -- custom tests from the calling package that should be performed. these are in the calling package's data-dir
  customDataDir :: String,  -- data-dir of the calling package. all tests are compiled and executed in their respective directory.
  allArguments :: [String], -- all different testable argument combinations for happy, as strings
  stopOnFailure :: Bool     -- continue with remaining tests after an error has occurred?
}

test :: TestSetup -> IO a
test setup = do
  hSetBuffering stdout NoBuffering -- required for cabal test
  defaultDir <- getDataDir
  let files = zip (repeat defaultDir) (defaultTests setup) ++
                zip (repeat (customDataDir setup)) (customTests setup)              -- (dir, file.ly)
  let tests = [(dir, file, arg) | (dir, file) <- files, arg <- allArguments setup]  -- (dir, file.ly, -ag)
  result <- test' tests (happyExec setup) (stopOnFailure setup)
  if result then exitSuccess else exitFailure

-- Perform the tests given in the list, specified via (directory, file, happy-options).
test' :: [(String, String, String)] -> String -> Bool -> IO Bool
test' [] _ _ = return True
test' ((dir, file, args):rest) happy stopOnFail = do
  result <- runSingleTest happy args dir file
  if result then test' rest happy stopOnFail
    else if stopOnFail
      then return False
      else do _ <- test' rest happy stopOnFail; return False

-- These test files do not use attribute grammars.
defaultTestFiles :: [String]
defaultTestFiles = ["Test.ly", "TestMulti.ly", "TestPrecedence.ly", "bug001.ly", "monad001.y", "monad002.ly", "precedence001.ly",
                    "precedence002.y", "bogus-token.y", "bug002.y", "Partial.ly", "issue91.y", "issue93.y", "issue94.y", "issue95.y",
                    "test_rules.y", "monaderror.y", "monaderror-explist.y", "typeclass_monad001.y", "typeclass_monad002.ly",
                    "typeclass_monad_lexer.y", "rank2.y", "shift01.y"]

attributeGrammarTestFiles :: [String]
attributeGrammarTestFiles = ["AttrGrammar001.y", "AttrGrammar002.y"]

defaultArguments :: [String]
defaultArguments = map ("--strict " ++) ["", "-a", "-g", "-ag", "-gc", "-agc"]

runSingleTest :: String -> String -> String -> String -> IO Bool
runSingleTest happy arguments dir testFile = do
  res <- runShell (do
    runCmdIn dir [happy, testFile, arguments, "-o", hsFile] True ||| failure
    runCmdIn dir ["ghc", "-Wall", hsFile, "-o", binFile] True ||| failure
    runCmd [dir </> binFile] True ||| failure
    )
  
  removeFiles
  return res
  where
    hsFile = basename testFile ++ ".hs"
    binFile = basename testFile ++ ".exe"
    hiFile = basename testFile ++ ".hi"
    oFile = basename testFile ++ ".o"

    removeFiles = do
      let generated = map (combine dir) [hsFile, binFile, hiFile, oFile]
      mapM_ tryRemovingFile generated

    failure = putStrLn $ "Test " ++ testFile ++ " failed!"

tryRemovingFile :: FilePath -> IO ()
tryRemovingFile file = do
  removeFile file `catchIO` const (return ())
  where
    catchIO :: IO a -> (IOError -> IO a) -> IO a
    catchIO = Control.Exception.catch

-- Only works for .y and .ly files.
basename :: FilePath -> FilePath
basename = reverse . basename' . reverse where
  basename' ('y':'l':'.':file) = file
  basename' ('y':'.':file) = file
  basename' file = error $ "Error: test file does not end in .y or .ly: " ++ reverse file