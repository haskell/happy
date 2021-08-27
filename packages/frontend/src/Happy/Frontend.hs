module Happy.Frontend (parseYFileContents, mangleAbsSyn, runFrontend, ParseResult, FrontendArgs(..), supportsParsingAttributeGrammars) where
  
import Happy.Grammar.Grammar
import Happy.Frontend.AbsSyn
import Happy.Frontend.Mangler
import Happy.Frontend.PrettyGrammar
import Happy.Frontend.Parser
import Happy.Frontend.ParseMonad.Class
import System.Exit
import System.IO
import Control.Monad.Except
import Control.Monad.Trans.Except

-------- Pure frontend functions, may be called without creating FrontendArgs --------

parseYFileContents :: String -> ParseResult AbsSyn
parseYFileContents contents = runFromStartP ourParser contents 1

mangleAbsSyn :: AbsSyn -> String -> ParseResult Grammar
mangleAbsSyn abssyn filename = first' unlines (mangler filename abssyn)
  where first' f (Left a) = Left (f a)
        first' _ (Right b) = Right b

-------- Main entry point (runFrontend) --------

data FrontendArgs = FrontendArgs {
  file :: String,
  prettyFile :: Maybe String,
  dumpMangle :: Bool
}

runFrontend :: FrontendArgs -> IO (Either String Grammar)
runFrontend args = runExceptT $ do
  _contents <- liftIO $ readFile file'
  (contents, name) <- liftIO $ possDelitify (reverse file') _contents
  abssyn <- except (parseYFileContents contents)
  liftIO $ writePrettyFile prettyFile' abssyn
  grammar <- except (mangleAbsSyn abssyn name)
  liftIO $ optPrint dumpMangle' (print grammar)
  return grammar
  where
      FrontendArgs { file = file', prettyFile = prettyFile', dumpMangle = dumpMangle' } = args
      optPrint b io = when b (putStr "\n---------------------\n" >> io)

-------- Helpers --------

writePrettyFile :: Maybe String -> AbsSyn -> IO ()
writePrettyFile location abssyn = do
  let out = render (ppAbsSyn abssyn) in
    case location of
      Just s   -> writeFile s out >>
        hPutStrLn stderr ("Production rules written to: " ++ s)
      Nothing  -> return ()

possDelitify :: String -> String -> IO (String, String)
possDelitify ('y':'l':'.':nm) fl = return (deLitify fl, reverse nm)
possDelitify ('y':'.':nm) fl     = return (fl, reverse nm)
possDelitify f            _      = die ("`" ++ reverse f ++ "' does not end in `.y' or `.ly'\n")
#if !MIN_VERSION_base(4,8,0)
  where die s = hPutStr stderr s >> exitWith (ExitFailure 1) 
#endif

deLitify :: String -> String
deLitify = deLit where
    deLit ('>':' ':r)   = deLit1 r
    deLit ('>':'\t':r)  = '\t' : deLit1 r
    deLit ('>':'\n':r)  = deLit r
    deLit ('>':_)       = error "Error when de-litify-ing"
    deLit ('\n':r)      = '\n' : deLit r
    deLit r             = deLit2 r
    deLit1 ('\n':r)     = '\n' : deLit r
    deLit1 (c:r)        = c : deLit1 r
    deLit1 []           = []
    deLit2 ('\n':r)     = '\n' : deLit r
    deLit2 (_:r)        = deLit2 r
    deLit2 []           = []

-------- Iff happy is built with bootstrapping, attribute grammars are supported --------

supportsParsingAttributeGrammars :: Bool
#ifdef HAPPY_BOOTSTRAP
supportsParsingAttributeGrammars = True
#else
supportsParsingAttributeGrammars = False
#endif