
{
module Parser (parse) where

import Lexer (lex_tok)
import ParserM (Token(..), Tree(..), ParserM, run_parser, get_pos, show_pos,
                happyError)
}

%name      parsex tree
%tokentype { Token }
%monad     { ParserM }
%lexer     { lex_tok } { TEOF }

%token
    fork { TFork }
    leaf { TLeaf }

%%

tree :: { Tree }
tree : leaf           { Leaf }
     | fork tree tree { Fork $2 $3 }

{
parse :: String -> Either String Tree
parse = run_parser parsex
}

