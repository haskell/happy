import Control.Monad
import Data.Foldable
import System.FilePath
import System.Directory
import System.Process
import System.Exit

main :: IO ()
main = do
  let bootstrap_root = "./bootstrap-root"
      happy_boot = bootstrap_root </> "happy"
  callProcess "cabal"
    [ "install", "happy",
      "-f", "-bootstrap",
      "--installdir=" ++ bootstrap_root ]
  with_y_files "packages" $ \y_file -> do
    putStrLn $ "Processing " ++ show y_file
    callProcess happy_boot [y_file]
    removeFile y_file
  removePathForcibly bootstrap_root

with_y_files :: FilePath -> (FilePath -> IO ()) -> IO ()
with_y_files path cont = do
  is_dir <- doesDirectoryExist path
  if is_dir then do
    entries <- listDirectory path
    for_ entries $ \entry ->
      with_y_files (path </> entry) cont
  else do
    is_file <- doesFileExist path
    if is_file then do
      let ext = takeExtension path
      when (ext == ".y" || ext == ".ly") (cont path)
    else do
      die ("Neither file nor dir: " ++ show path)
