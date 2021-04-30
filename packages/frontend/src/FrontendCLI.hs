module FrontendCLI(FrontendOpts(..), runFrontend) where

import AbsSyn
import Grammar
import Frontend
import PrettyGrammar
import System.IO
import GenUtils
import Control.Monad.Except
import Control.Monad.Trans.Except

data FrontendOpts = FrontendOpts {
  file :: String,
  prettyFile :: Maybe String
}

runFrontend :: FrontendOpts -> IO (Either String Grammar)
runFrontend opts = runExceptT $ do
    let FrontendOpts { file = file, prettyFile = prettyFile } = opts in do
        _contents <- liftIO $ readFile file
        (contents, name) <- liftIO $ possDelit (reverse file) _contents
        abssyn <- except (parseYFileContents contents)
        liftIO $ writePrettyFile prettyFile abssyn
        except (mangleAbsSyn abssyn name)

writePrettyFile :: Maybe String -> AbsSyn -> IO ()
writePrettyFile location abssyn = do
  let out = render (ppAbsSyn abssyn) in
    case location of
      Just s   -> writeFile s out >>
        hPutStrLn stderr ("Production rules written to: " ++ s)
      Nothing  -> return ()

possDelit :: String -> String -> IO (String,String)
possDelit ('y':'l':'.':nm) fl = return (deLitify fl,reverse nm)
possDelit ('y':'.':nm) fl     = return (fl,reverse nm)
possDelit f            _      =
      dieHappy ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")

deLitify :: String -> String
deLitify = deLit
 where
      deLit ('>':' ':r)  = deLit1 r
      deLit ('>':'\t':r)  = '\t' : deLit1 r
      deLit ('>':'\n':r)  = deLit r
      deLit ('>':_)  = error "Error when de-litify-ing"
      deLit ('\n':r) = '\n' : deLit r
      deLit r        = deLit2 r
      deLit1 ('\n':r) = '\n' : deLit r
      deLit1 (c:r)    = c : deLit1 r
      deLit1 []       = []
      deLit2 ('\n':r) = '\n' : deLit r
      deLit2 (_:r)    = deLit2 r
      deLit2 []       = []