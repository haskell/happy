import Data.List (intercalate)
import GHC.Conc (numCapabilities)
import System.Process (system)
import System.Exit (exitWith)

main = do
  let jFlag = "-j" ++ show numCapabilities -- to run tests in parallel, run `cabal test --test-options="+RTS -N"`
  let cmd = ["make", jFlag, "-k", "-C", "tests", "clean", "all"]
  system (intercalate " " cmd) >>= exitWith
