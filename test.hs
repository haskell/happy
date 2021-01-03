import Data.List (intercalate)
import GHC.Conc (numCapabilities)
import System.Process (system)
import System.Exit (exitWith)

main = do
  let jFlag = "-j" ++ show numCapabilities
  let cmd = ["make", jFlag, "-k", "-C", "tests", "clean", "all"]
  system (intercalate " " cmd) >>= exitWith
