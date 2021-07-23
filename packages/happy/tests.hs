import Happy.Test
import Happy.Frontend
import Paths_happy

main = do
  dir <- getDataDir
  let tests = defaultTestFiles ++
              (if supportsParsingAttributeGrammars then attributeGrammarTestFiles else [])
  let setup = TestSetup {
    happyExec = "happy",
    defaultTests = tests,
    customTests = [],
    customDataDir = dir,
    allArguments = map ("--strict " ++) ["", "-a", "-g", "-ag", "-gc", "-agc"],
    stopOnFailure = True
  }
  test setup