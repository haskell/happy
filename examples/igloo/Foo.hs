
module Main (main) where

import Parser (parse)
import IO (hPutStrLn, stderr)

main :: IO ()
main = do x <- getContents
          case parse x of
              Left e -> hPutStrLn stderr $ "Failed with: " ++ e
              Right t -> print t

