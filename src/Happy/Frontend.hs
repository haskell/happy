module Happy.Frontend where

import Happy.Frontend.AbsSyn
import Happy.Frontend.Parser
import Happy.Frontend.ParseMonad.Class

parseYFileContents :: String -> ParseResult BookendedAbsSyn
parseYFileContents contents = runFromStartP ourParser contents 1

data FileType = Y | LY

fileNameAndType :: String -> Maybe (String, FileType)
fileNameAndType = nameType . reverse where
  nameType ('y':'.':f) = Just (reverse f, Y)
  nameType ('y':'l':'.':f) = Just (reverse f, LY)
  nameType _ = Nothing

-- Delit, converting an ly file into a y file.
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
