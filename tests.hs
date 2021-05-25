import Test
import Paths_happy
import Frontend

main = do
  dir <- getDataDir
  let tests = defaultTestFiles ++ (if isBootstrapped then attributeGrammarTestFiles else [])
  let setup = TestSetup {
    happyExec = "happy",
    defaultTests = tests,
    customTests = [],
    customDataDir = dir,
    allArguments = defaultArguments,
    stopOnFailure = True
  }
  test setup