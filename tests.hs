import Happy.Test
import Happy.Frontend
import Paths_happy

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