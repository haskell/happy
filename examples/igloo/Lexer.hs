{-# OPTIONS -cpp #-}
{-# LINE 2 "Lexer.x" #-}
module Lexer (lex_tok) where

import Control.Monad.State (StateT, get)
import ParserM (ParserM (..), mkT, Token(..), St, start_code,
                StartCode, Action, set_start_code,
                show_pos, position, input,
                AlexInput, alexGetChar, alexInputPrevChar)

#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
alex_base :: Array Int Int
alex_base = listArray (0,10) [-8,-3,2,0,-91,-97,-93,0,-83,-77,-80]

alex_table :: Array Int Int
alex_table = listArray (0,257) [0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,4,5,7,8,9,0,0,2,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,257) [-1,9,10,11,12,13,9,10,11,12,13,9,10,11,12,13,107,114,111,102,97,101,-1,-1,32,-1,-1,-1,-1,32,-1,-1,-1,-1,32,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,102,-1,-1,-1,-1,-1,108,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,10) [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,10) [[],[],[(AlexAccSkip)],[(AlexAcc (alex_action_1))],[],[],[],[(AlexAcc (alex_action_2))],[],[],[]]
{-# LINE 18 "Lexer.x" #-}
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

alex_action_1 = mkT TFork 
alex_action_2 = mkT TLeaf 
{-# LINE 1 "GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 22 "GenericTemplate.hs" #-}























{-# LINE 66 "GenericTemplate.hs" #-}

alexIndexShortOffAddr arr off = arr ! off


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> Maybe (AlexInput,Int,act)
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  case s of 
    (-1) -> (last_acc, input)
    _ -> alex_scan_tkn' user orig_input len input s last_acc

alex_scan_tkn' user orig_input len input s last_acc =
  let 
	new_acc = check_accs (alex_accept `unsafeAt` (s))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexShortOffAddr alex_base s
		(ord_c) = ord c
		offset = (base + ord_c)
		check  = alexIndexShortOffAddr alex_check offset
		
		new_s = if (offset >= (0)) && (check == ord_c)
			  then alexIndexShortOffAddr alex_table offset
			  else alexIndexShortOffAddr alex_deflt s
	in
	alex_scan_tkn user orig_input (len + (1)) new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i
