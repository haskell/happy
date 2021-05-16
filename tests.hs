import Test
import Paths_happy
import Frontend

main = do
  dir <- getDataDir
  let tests = if isBootstrapped then defaultTestFiles ++ bootstrapTestFiles else defaultTestFiles
  let setup = TestSetup {
    happyExec = "happy",
    defaultTests = tests,
    customTests = [],
    customDataDir = dir,
    allArguments = defaultArguments,
    stopOnFailure = True
  }
  test setup