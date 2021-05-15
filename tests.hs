import Test
import Paths_happy
import Frontend

main = do
  dir <- getDataDir
  let tests = if isBootstrapped then defaultTestFiles else defaultTestFiles ++ bootstrapTestFiles
  let setup = TestSetup { happyExec = "happy", defaultTests = tests, customTests = [], customDataDir = dir, allArguments = defaultArguments }
  test setup