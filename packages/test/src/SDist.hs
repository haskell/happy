module SDist(sdist_test) where

import Shell
import System.Process
import System.IO.Error
import System.Exit
import Data.Ord
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.IO.Class

-- Test whether the tarballs are distribution-ready by calling `cabal sdist`, merging the tarballs into one umbrella directory and building and testing in this directory.
-- Arguments:
--  * absolute location of main project directory
--  * name of the main executable (which also provides a test suite)
--  * names of all required local packages (including the main executable)
--  * perform bootstrapping-testing? if true, first build without bootstrap, and then use the generated binary for bootstrapping.
sdist_test :: String -> String -> [String] -> Bool -> IO ()
sdist_test a b c d = do
  success <- runShell $ sdist_test' a b c d
  if success
    then putStrLn "Success! The tarballs inside dist-newstyle/sdist are now ready for distribution." >> exitSuccess
    else putStrLn "Failure." >> exitFailure

sdist_test' :: String -> String -> [String] -> Bool -> Shell
sdist_test' projectDir executable localPackages bootstrapping = do
  let sdistDir = concatPaths projectDir "dist-newstyle/sdist"

  -- `cabal sdist all` -> returns `package-name-VERSION` for each package-name
  fullNames <- cabalSdistAll localPackages sdistDir

  liftIO . putStrLn $ "packages: " ++ show (zip localPackages fullNames)

  -- inside dist-newstyle/sdist:
  -- `rm -rf package-name`
  -- `tar -xf package-name.tar.gz`
  -- `rm -rf umbrella; mkdir umbrella`
  -- `mv package-name umbrella`
  let rm = \name -> runCmdIn sdistDir ["rm", "-rf", name] False
  let untar = \name -> runCmdIn sdistDir ["tar", "-xf", name ++ targz] False
  let mkUmbrella = runCmdIn sdistDir ["rm -rf umbrella; mkdir umbrella"] False
  let mv = \name -> runCmdIn sdistDir ["mv", name, "umbrella"] False

  -- inside umbrella:
  -- `echo "packages: ..." > cabal.project`
  let umbrellaDir = concatPaths sdistDir "umbrella"
  let packagesText = intercalate "\n  " (["packages:"] ++ fullNames) ++ "\ntests: True"
  let genCabalProj = runCmdIn umbrellaDir ["echo", "'" ++ packagesText ++ "'", ">", "cabal.project"] False

  -- Perform commands in sequence, stop on error
  sequence_ (map rm fullNames)
  sequence_ (map untar fullNames)
  mkUmbrella
  sequence_ (map mv fullNames)
  genCabalProj

  liftIO . putStrLn $ "Umbrella dir (" ++ umbrellaDir ++ ") generated successfully."

  if bootstrapping
    then testWithBootstrapping umbrellaDir executable
    else testWithoutBootstrapping umbrellaDir executable

  return ()

testWithoutBootstrapping :: FilePath -> String -> Shell
testWithoutBootstrapping dir executable = do
  runCmdIn dir ["cabal", "build", executable] False
  runCmdIn dir ["cabal", "test", executable] False

testWithBootstrapping :: FilePath -> String -> Shell
testWithBootstrapping dir executable = do
  runCmdIn dir ["cabal", "build", executable, "-f", "-bootstrap"] False
  runCmdIn dir ["cabal", "install", executable, "-f", "-bootstrap", "--installdir=./bootstrap-root"] False
  runCmdIn dir ["cabal", "test", executable, "-f", "-bootstrap"] False

  -- We now want our just-built happy to be used for bootstrapping happy, i.e. building happy's .ly files.
  -- Using `cabal build --with-happy=` (instead of exporting ./bootstrap-root to PATH) also allows using happy's with a different name like `happy-rad`.
  let bootstrapHappy = concatPaths dir $ concatPaths "bootstrap-root" executable
  runCmdIn dir ["cabal", "build", executable, "-f", "+bootstrap", "--with-happy=" ++ bootstrapHappy] False
  runCmdIn dir ["cabal", "test", executable, "-f", "+bootstrap"] False

-- Perform `cabal sdist all` and match the output lines to the given package names.
-- This is required to extract the full package name (i.e. package-name-VERSION) for each package.
-- This is less elegant than performing `cabal sdist package` for each package on its own, but is required because `cabal sdist happy` doesn't work on its own - `cabal sdist all` does.
cabalSdistAll :: [String] -> String -> TypedShell [String]
cabalSdistAll packageNames baseDir = do
  output <- liftIO $ readCreateProcess ((shell "cabal sdist all") { cwd = Just baseDir }) "" `catchIOError` const (return "")
  let fullNames = catMaybes . catMaybes $ map extractFullName $ lines output
  let matched = catMaybes $ map (bestMatch fullNames) packageNames
  if length packageNames == length matched then return matched else empty
  where
    -- Find package-name-VERSION matching to package-name.
    -- Note: we cannot just use `isPrefixOf` because then `happy` would match to `happy-frontend-1.21.0`!
    bestMatch fullNames packageName = head' $ sortBy (comparing numPrefixMatches) prefixMatches where
      prefixMatches = filter (isPrefixOf packageName) fullNames
      numPrefixMatches fullName = length $ filter (flip isPrefixOf fullName) packageNames

    -- extract "package-name-VERSION" from a string containg "package-name-VERSION.tar.gz"
    extractFullName output =
      case (lastSubstring baseDir output, lastSubstring targz output) of
        (Just i', Just j) -> let i = i' + 1 + length baseDir in return . Just $ take (j-i) $ drop i $ output
        _ -> return Nothing
    
    -- the returned index is counted from the back so you can use `drop` with the result
    lastSubstring search str = last' $ findIndices (isPrefixOf search) (tails str)

    head' x = if null x then Nothing else Just (head x)
    last' x = if null x then Nothing else Just (last x)

targz :: String
targz = ".tar.gz"