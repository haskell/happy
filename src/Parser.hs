-- parser produced by Happy Version 1.1-simonm


module Parser (ourParser,AbsSyn) where
import ParseMonad
import GenUtils
import AbsSyn
import Lexer

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn1(AbsSyn)
	| HappyAbsSyn2([(String, [([String],String,Int)], Maybe String)])
	| HappyAbsSyn3((String, [([String],String,Int)], Maybe String))
	| HappyAbsSyn4([([String],String,Int)])
	| HappyAbsSyn5(([String],String,Int))
	| HappyAbsSyn6([Directive String])
	| HappyAbsSyn7(Directive String)
	| HappyAbsSyn8([(String,String)])
	| HappyAbsSyn9((String,String))
	| HappyAbsSyn10([String])
	| HappyAbsSyn11(String)
	| HappyAbsSyn12(Maybe String)

type HappyReduction = 
	   Int 
	-> (Token)
	-> HappyState (Token) ([HappyAbsSyn] -> P(AbsSyn))
	-> [HappyState (Token) ([HappyAbsSyn] -> P(AbsSyn))] 
	-> [HappyAbsSyn] 
	-> P(AbsSyn)

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48 :: Int -> HappyReduction

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26 :: HappyReduction

action_0 (20) = happyShift action_4
action_0 (1) = happyGoto action_1
action_0 (12) = happyGoto action_2
action_0 (13) = happyGoto action_3
action_0 _ = happyReduce_25

action_1 (33) = happyAccept
action_1 _ = happyFail

action_2 (15) = happyShift action_7
action_2 (16) = happyShift action_8
action_2 (17) = happyShift action_9
action_2 (18) = happyShift action_10
action_2 (19) = happyShift action_11
action_2 (6) = happyGoto action_5
action_2 (7) = happyGoto action_6
action_2 _ = happyFail

action_3 _ = happyReduce_24

action_4 _ = happyReduce_26

action_5 (15) = happyShift action_7
action_5 (16) = happyShift action_8
action_5 (17) = happyShift action_9
action_5 (18) = happyShift action_10
action_5 (19) = happyShift action_11
action_5 (24) = happyShift action_21
action_5 (7) = happyGoto action_20
action_5 _ = happyFail

action_6 _ = happyReduce_12

action_7 (20) = happyShift action_4
action_7 (13) = happyGoto action_19
action_7 _ = happyFail

action_8 (14) = happyShift action_15
action_8 (8) = happyGoto action_16
action_8 (9) = happyGoto action_17
action_8 (11) = happyGoto action_18
action_8 _ = happyFail

action_9 (14) = happyShift action_15
action_9 (11) = happyGoto action_14
action_9 _ = happyFail

action_10 (20) = happyShift action_4
action_10 (13) = happyGoto action_13
action_10 _ = happyFail

action_11 (20) = happyShift action_4
action_11 (13) = happyGoto action_12
action_11 _ = happyFail

action_12 (20) = happyShift action_4
action_12 (13) = happyGoto action_28
action_12 _ = happyFail

action_13 (20) = happyShift action_4
action_13 (13) = happyGoto action_27
action_13 _ = happyFail

action_14 _ = happyReduce_15

action_15 _ = happyReduce_23

action_16 _ = happyReduce_14

action_17 (14) = happyShift action_15
action_17 (8) = happyGoto action_26
action_17 (9) = happyGoto action_17
action_17 (11) = happyGoto action_18
action_17 _ = happyReduce_19

action_18 (20) = happyShift action_4
action_18 (13) = happyGoto action_25
action_18 _ = happyFail

action_19 _ = happyReduce_13

action_20 _ = happyReduce_11

action_21 (14) = happyShift action_15
action_21 (2) = happyGoto action_22
action_21 (3) = happyGoto action_23
action_21 (11) = happyGoto action_24
action_21 _ = happyFail

action_22 (14) = happyShift action_15
action_22 (20) = happyShift action_4
action_22 (3) = happyGoto action_32
action_22 (11) = happyGoto action_24
action_22 (12) = happyGoto action_33
action_22 (13) = happyGoto action_3
action_22 _ = happyReduce_25

action_23 _ = happyReduce_3

action_24 (21) = happyShift action_30
action_24 (23) = happyShift action_31
action_24 _ = happyFail

action_25 _ = happyReduce_20

action_26 _ = happyReduce_18

action_27 _ = happyReduce_16

action_28 (20) = happyShift action_4
action_28 (13) = happyGoto action_29
action_28 _ = happyFail

action_29 _ = happyReduce_17

action_30 (14) = happyShift action_15
action_30 (4) = happyGoto action_35
action_30 (5) = happyGoto action_36
action_30 (10) = happyGoto action_37
action_30 (11) = happyGoto action_38
action_30 _ = happyReduce_22

action_31 (20) = happyShift action_4
action_31 (13) = happyGoto action_34
action_31 _ = happyFail

action_32 _ = happyReduce_2

action_33 _ = happyReduce_1

action_34 (14) = happyShift action_15
action_34 (21) = happyShift action_43
action_34 (11) = happyGoto action_42
action_34 _ = happyFail

action_35 _ = happyReduce_6

action_36 (25) = happyShift action_41
action_36 _ = happyReduce_8

action_37 (20) = happyShift action_4
action_37 (13) = happyGoto action_40
action_37 _ = happyFail

action_38 (14) = happyShift action_15
action_38 (10) = happyGoto action_39
action_38 (11) = happyGoto action_38
action_38 _ = happyReduce_22

action_39 _ = happyReduce_21

action_40 (22) = happyShift action_47
action_40 _ = happyReduce_10

action_41 (14) = happyShift action_15
action_41 (4) = happyGoto action_46
action_41 (5) = happyGoto action_36
action_41 (10) = happyGoto action_37
action_41 (11) = happyGoto action_38
action_41 _ = happyReduce_22

action_42 (21) = happyShift action_45
action_42 _ = happyFail

action_43 (14) = happyShift action_15
action_43 (4) = happyGoto action_44
action_43 (5) = happyGoto action_36
action_43 (10) = happyGoto action_37
action_43 (11) = happyGoto action_38
action_43 _ = happyReduce_22

action_44 _ = happyReduce_4

action_45 (14) = happyShift action_15
action_45 (4) = happyGoto action_48
action_45 (5) = happyGoto action_36
action_45 (10) = happyGoto action_37
action_45 (11) = happyGoto action_38
action_45 _ = happyReduce_22

action_46 _ = happyReduce_7

action_47 _ = happyReduce_9

action_48 _ = happyReduce_5

happyReduce_1 = happyReduce 5 1 reduction where {
  reduction
	((HappyAbsSyn12  happy_var_5) :
	(HappyAbsSyn2  happy_var_4) :
	_ :
	(HappyAbsSyn6  happy_var_2) :
	(HappyAbsSyn12  happy_var_1) :
	happyRest)
	 = HappyAbsSyn1
		 (AbsSyn happy_var_1 (reverse happy_var_2) (reverse happy_var_4) happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_2 = happySpecReduce_2 2 reduction where {
  reduction
	(HappyAbsSyn3  happy_var_2)
	(HappyAbsSyn2  happy_var_1)
	 =  HappyAbsSyn2
		 (happy_var_2 : happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_3 = happySpecReduce_1 2 reduction where {
  reduction
	(HappyAbsSyn3  happy_var_1)
	 =  HappyAbsSyn2
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_4 = happyReduce 5 3 reduction where {
  reduction
	((HappyAbsSyn4  happy_var_5) :
	_ :
	(HappyAbsSyn11  happy_var_3) :
	_ :
	(HappyAbsSyn11  happy_var_1) :
	happyRest)
	 = HappyAbsSyn3
		 ((happy_var_1,happy_var_5,Just happy_var_3)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_5 = happyReduce 6 3 reduction where {
  reduction
	((HappyAbsSyn4  happy_var_6) :
	_ :
	_ :
	(HappyAbsSyn11  happy_var_3) :
	_ :
	(HappyAbsSyn11  happy_var_1) :
	happyRest)
	 = HappyAbsSyn3
		 ((happy_var_1,happy_var_6,Just happy_var_3)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_6 = happySpecReduce_3 3 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn3
		 ((happy_var_1,happy_var_3,Nothing));
  reduction _ _ _  = notHappyAtAll }

happyReduce_7 = happySpecReduce_3 4 reduction where {
  reduction
	(HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_8 = happySpecReduce_1 4 reduction where {
  reduction
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_9 = happyMonadReduce 3 5 HappyAbsSyn5 reduction where {
  reduction
	(_ :
	(HappyAbsSyn11  happy_var_2) :
	(HappyAbsSyn10  happy_var_1) :
	happyRest)
	 =  \s l -> returnP (happy_var_1,happy_var_2,l) s l;
  reduction _ = notHappyAtAll }

happyReduce_10 = happyMonadReduce 2 5 HappyAbsSyn5 reduction where {
  reduction
	((HappyAbsSyn11  happy_var_2) :
	(HappyAbsSyn10  happy_var_1) :
	happyRest)
	 =  \s l -> returnP (happy_var_1,happy_var_2,l) s l;
  reduction _ = notHappyAtAll }

happyReduce_11 = happySpecReduce_2 6 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 : happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_12 = happySpecReduce_1 6 reduction where {
  reduction
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_13 = happySpecReduce_2 7 reduction where {
  reduction
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TokenType happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_14 = happySpecReduce_2 7 reduction where {
  reduction
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TokenSpec happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_15 = happySpecReduce_2 7 reduction where {
  reduction
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TokenName happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_16 = happySpecReduce_3 7 reduction where {
  reduction
	(HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TokenLexer happy_var_2 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_17 = happyReduce 4 7 reduction where {
  reduction
	((HappyAbsSyn11  happy_var_4) :
	(HappyAbsSyn11  happy_var_3) :
	(HappyAbsSyn11  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn7
		 (TokenMonad happy_var_2 happy_var_3 happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_18 = happySpecReduce_2 8 reduction where {
  reduction
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_19 = happySpecReduce_1 8 reduction where {
  reduction
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_20 = happySpecReduce_2 9 reduction where {
  reduction
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 ((happy_var_1,happy_var_2));
  reduction _ _  = notHappyAtAll }

happyReduce_21 = happySpecReduce_2 10 reduction where {
  reduction
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_22 = happySpecReduce_0 10 reduction where {
  reduction
	 =  HappyAbsSyn10
		 ([])}

happyReduce_23 = happySpecReduce_1 11 reduction where {
  reduction
	(HappyTerminal (TokenInfo happy_var_1 TokId))
	 =  HappyAbsSyn11
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_24 = happySpecReduce_1 12 reduction where {
  reduction
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (Just happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_25 = happySpecReduce_0 12 reduction where {
  reduction
	 =  HappyAbsSyn12
		 (Nothing)}

happyReduce_26 = happySpecReduce_1 13 reduction where {
  reduction
	(HappyTerminal (TokenInfo happy_var_1 TokCodeQuote))
	 =  HappyAbsSyn11
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 33 33 (error "reading EOF!") (HappyState action) sts stk;
	TokenInfo _ TokId -> cont 14;
	TokenKW     TokSpecId_TokenType -> cont 15;
	TokenKW     TokSpecId_Token -> cont 16;
	TokenKW     TokSpecId_Name -> cont 17;
	TokenKW     TokSpecId_Lexer -> cont 18;
	TokenKW     TokSpecId_Monad -> cont 19;
	TokenInfo _ TokCodeQuote -> cont 20;
	TokenKW     TokColon -> cont 21;
	TokenKW     TokSemiColon -> cont 22;
	TokenKW     TokDoubleColon -> cont 23;
	TokenKW     TokDoublePercent -> cont 24;
	TokenKW     TokBar -> cont 25;
	TokenKW     TokOpenBrack -> cont 26;
	TokenKW     TokCloseBrack -> cont 27;
	TokenKW     TokMinus -> cont 28;
	TokenKW     TokPlus -> cont 29;
	TokenKW     TokStar -> cont 30;
	TokenKW     TokQmark -> cont 31;
	TokenKW     TokBackQ -> cont 32;
	})

happyThen = thenP
happyReturn = returnP
ourParser = happyParse



happyError :: P a
happyError s l = failP (show l ++ ": Parse error\n") s l

-- $Id: Parser.hs,v 1.5 1997/03/28 14:56:28 simonm Exp $

{-
	The stack is in the following order throughout the parse:

	i	current token number
	j	another copy of this to avoid messing with the stack
	tk	current token semantic value
	st	current state
	sts	state stack
	stk	semantic stack
-}

-----------------------------------------------------------------------------

happyParse = happyNewToken action_0 [] []

-- All this HappyState stuff is simply because we can't have recursive
-- types in Haskell without an intervening data structure.

data HappyState b c = HappyState
        (Int ->                         -- token number
         Int ->                         -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts [ HappyAbsSyn1 ans ] = happyReturn ans
happyAccept j tk st sts _                    = notHappyAtAll

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (-1) tk st sts stk@(HappyErrorToken i : _) =
--     _trace "shifting the error token" $
     new_state i i tk (HappyState new_state) (st:sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (st:sts) (HappyTerminal tk:stk)

-----------------------------------------------------------------------------
-- Reducing

-- happyReduce is specialised for the common cases.

-- don't allow reductions when we're in error recovery, because this can
-- lead to an infinite loop.

happySpecReduce_0 i fn (-1) tk _ sts stk
     = case sts of
	st@(HappyState action):sts -> action (-1) (-1) tk st sts stk
	_ -> happyError
happySpecReduce_0 i fn j tk st@(HappyState action) sts stk
     = action i j tk st (st:sts) (fn : stk)

happySpecReduce_1 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_1 i fn j tk _ sts@(st@(HappyState action):_) (v1:stk')
     = action i j tk st sts (fn v1 : stk')

happySpecReduce_2 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_2 i fn j tk _ (_:sts@(st@(HappyState action):_)) (v1:v2:stk')
     = action i j tk st sts (fn v1 v2 : stk')

happySpecReduce_3 i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happySpecReduce_3 i fn j tk _ (_:_:sts@(st@(HappyState action):_)) 
	(v1:v2:v3:stk')
     = action i j tk st sts (fn v1 v2 v3 : stk')

happyReduce k i fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happyReduce k i fn j tk st sts stk = action i j tk st' sts' (fn stk)
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)

happyMonadReduce k i c fn (-1) tk _ (st@(HappyState action):sts) stk
     = action (-1) (-1) tk st sts stk
happyMonadReduce k i c fn j tk st sts stk =
	happyThen (fn stk) (\r -> action i j tk st' sts' (c r : stk'))
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)
	     stk' = drop (k::Int) stk

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto action j tk st = action j j tk (HappyState action)

-----------------------------------------------------------------------------
-- Error recovery (-1 is the error token)

-- fail if we are in recovery and no more states to discard
happyFail  (-1) tk st' [] stk = happyError

-- discard a state
happyFail  (-1) tk st' (st@(HappyState action):sts) stk =
--	_trace "discarding state" $
	action (-1) (-1) tk st sts stk

-- Enter error recovery: generate an error token,
-- 			 save the old token and carry on.

-- we push the error token on the stack in anticipation of a shift,
-- and also because this is a convenient place to store the saved token.

happyFail  i tk st@(HappyState action) sts stk =
--	_trace "entering error recovery" $
	action (-1) (-1) tk st sts (HappyErrorToken i : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-- end of Happy Template.
