module Frontend (parseYFileContents, mangleAbsSyn, Grammar(..), AbsSyn(..)) where
  
import AbsSyn
import Grammar
import Data.Bifunctor
import Parser
import ParseMonad
import ParseMonad.Class

parseYFileContents :: String -> ParseResult AbsSyn
parseYFileContents contents = runFromStartP ourParser contents 1

mangleAbsSyn :: AbsSyn -> String -> ParseResult Grammar
mangleAbsSyn abssyn filename = first unlines (mangler filename abssyn)