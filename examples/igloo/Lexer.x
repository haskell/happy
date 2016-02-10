
{
module Lexer (lex_tok) where

import Control.Monad.State (StateT, get)
import ParserM (ParserM (..), mkT, Token(..), St, start_code,
                StartCode, Action, set_start_code,
                show_pos, position, input,
                AlexInput, alexGetByte, alexInputPrevChar)
}

words :-

 <0>  $white+  ;
 <0>  fork     { mkT TFork }
 <0>  leaf     { mkT TLeaf }

{
get_tok :: AlexInput -> StateT St (Either String) (Token, AlexInput)
get_tok = \i ->
   do st <- get
      case alexScan i (start_code st) of
          AlexEOF -> return (TEOF, i)
          AlexError _ -> fail $ "Lexical error at " ++ show_pos (position i)
          AlexSkip i' _ -> get_tok i'
          AlexToken i' l a -> a (i', take l (input i))

begin :: StartCode -> Action
begin sc (i, _) = do set_start_code sc
                     get_tok i

lex_tok :: (Token -> ParserM a) -> ParserM a
lex_tok cont = ParserM $ \i ->
   do (tok, iz) <- get_tok i
      case cont tok of
          ParserM x -> x iz
}

