-- parser produced by Happy Version 1.6


module Parser (ourParser,AbsSyn) where
import ParseMonad
import GenUtils
import AbsSyn
import Lexer

#ifdef __GLASGOW_HASKELL__
import GlaExts
#endif


action_0 (23#) = happyShift action_4
action_0 (4#) = happyGoto action_1
action_0 (15#) = happyGoto action_2
action_0 (16#) = happyGoto action_3
action_0 x = happyTcHack x happyReduce_26

action_1 (29#) = happyAccept
action_1 x = happyTcHack x happyFail

action_2 (18#) = happyShift action_7
action_2 (19#) = happyShift action_8
action_2 (20#) = happyShift action_9
action_2 (21#) = happyShift action_10
action_2 (22#) = happyShift action_11
action_2 (9#) = happyGoto action_5
action_2 (10#) = happyGoto action_6
action_2 x = happyTcHack x happyFail

action_3 x = happyTcHack x happyReduce_25

action_4 x = happyTcHack x happyReduce_27

action_5 (18#) = happyShift action_7
action_5 (19#) = happyShift action_8
action_5 (20#) = happyShift action_9
action_5 (21#) = happyShift action_10
action_5 (22#) = happyShift action_11
action_5 (27#) = happyShift action_21
action_5 (10#) = happyGoto action_20
action_5 x = happyTcHack x happyFail

action_6 x = happyTcHack x happyReduce_12

action_7 (23#) = happyShift action_4
action_7 (16#) = happyGoto action_19
action_7 x = happyTcHack x happyFail

action_8 (17#) = happyShift action_15
action_8 (11#) = happyGoto action_16
action_8 (12#) = happyGoto action_17
action_8 (14#) = happyGoto action_18
action_8 x = happyTcHack x happyFail

action_9 (17#) = happyShift action_15
action_9 (14#) = happyGoto action_14
action_9 x = happyTcHack x happyFail

action_10 (23#) = happyShift action_4
action_10 (16#) = happyGoto action_13
action_10 x = happyTcHack x happyFail

action_11 (23#) = happyShift action_4
action_11 (16#) = happyGoto action_12
action_11 x = happyTcHack x happyFail

action_12 (23#) = happyShift action_4
action_12 (16#) = happyGoto action_28
action_12 x = happyTcHack x happyReduce_17

action_13 (23#) = happyShift action_4
action_13 (16#) = happyGoto action_27
action_13 x = happyTcHack x happyFail

action_14 x = happyTcHack x happyReduce_15

action_15 x = happyTcHack x happyReduce_24

action_16 x = happyTcHack x happyReduce_14

action_17 (17#) = happyShift action_15
action_17 (11#) = happyGoto action_26
action_17 (12#) = happyGoto action_17
action_17 (14#) = happyGoto action_18
action_17 x = happyTcHack x happyReduce_20

action_18 (23#) = happyShift action_4
action_18 (16#) = happyGoto action_25
action_18 x = happyTcHack x happyFail

action_19 x = happyTcHack x happyReduce_13

action_20 x = happyTcHack x happyReduce_11

action_21 (17#) = happyShift action_15
action_21 (5#) = happyGoto action_22
action_21 (6#) = happyGoto action_23
action_21 (14#) = happyGoto action_24
action_21 x = happyTcHack x happyFail

action_22 (17#) = happyShift action_15
action_22 (23#) = happyShift action_4
action_22 (6#) = happyGoto action_32
action_22 (14#) = happyGoto action_24
action_22 (15#) = happyGoto action_33
action_22 (16#) = happyGoto action_3
action_22 x = happyTcHack x happyReduce_26

action_23 x = happyTcHack x happyReduce_3

action_24 (24#) = happyShift action_30
action_24 (26#) = happyShift action_31
action_24 x = happyTcHack x happyFail

action_25 x = happyTcHack x happyReduce_21

action_26 x = happyTcHack x happyReduce_19

action_27 x = happyTcHack x happyReduce_16

action_28 (23#) = happyShift action_4
action_28 (16#) = happyGoto action_29
action_28 x = happyTcHack x happyFail

action_29 x = happyTcHack x happyReduce_18

action_30 (17#) = happyShift action_15
action_30 (7#) = happyGoto action_35
action_30 (8#) = happyGoto action_36
action_30 (13#) = happyGoto action_37
action_30 (14#) = happyGoto action_38
action_30 x = happyTcHack x happyReduce_23

action_31 (23#) = happyShift action_4
action_31 (16#) = happyGoto action_34
action_31 x = happyTcHack x happyFail

action_32 x = happyTcHack x happyReduce_2

action_33 x = happyTcHack x happyReduce_1

action_34 (17#) = happyShift action_15
action_34 (24#) = happyShift action_43
action_34 (14#) = happyGoto action_42
action_34 x = happyTcHack x happyFail

action_35 x = happyTcHack x happyReduce_6

action_36 (28#) = happyShift action_41
action_36 x = happyTcHack x happyReduce_8

action_37 (23#) = happyShift action_4
action_37 (16#) = happyGoto action_40
action_37 x = happyTcHack x happyFail

action_38 (17#) = happyShift action_15
action_38 (13#) = happyGoto action_39
action_38 (14#) = happyGoto action_38
action_38 x = happyTcHack x happyReduce_23

action_39 x = happyTcHack x happyReduce_22

action_40 (25#) = happyShift action_47
action_40 x = happyTcHack x happyReduce_10

action_41 (17#) = happyShift action_15
action_41 (7#) = happyGoto action_46
action_41 (8#) = happyGoto action_36
action_41 (13#) = happyGoto action_37
action_41 (14#) = happyGoto action_38
action_41 x = happyTcHack x happyReduce_23

action_42 (24#) = happyShift action_45
action_42 x = happyTcHack x happyFail

action_43 (17#) = happyShift action_15
action_43 (7#) = happyGoto action_44
action_43 (8#) = happyGoto action_36
action_43 (13#) = happyGoto action_37
action_43 (14#) = happyGoto action_38
action_43 x = happyTcHack x happyReduce_23

action_44 x = happyTcHack x happyReduce_4

action_45 (17#) = happyShift action_15
action_45 (7#) = happyGoto action_48
action_45 (8#) = happyGoto action_36
action_45 (13#) = happyGoto action_37
action_45 (14#) = happyGoto action_38
action_45 x = happyTcHack x happyReduce_23

action_46 x = happyTcHack x happyReduce_7

action_47 x = happyTcHack x happyReduce_9

action_48 x = happyTcHack x happyReduce_5

happyReduce_1 = happyReduce 5# 4# (unsafeCoerce# reduction) where {
  reduction
	(happy_x_5 :
	happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	_ = (unsafeCoerce# happy_x_3); 
	happy_var_4 = (unsafeCoerce# happy_x_4); 
	happy_var_5 = (unsafeCoerce# happy_x_5); 
	} in
		
		 (AbsSyn happy_var_1 (reverse happy_var_2) (reverse happy_var_4) happy_var_5) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_2 = happySpecReduce_2 5# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (happy_var_2 : happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_3 = happySpecReduce_1 5# (unsafeCoerce# reduction) where {
  reduction
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	} in
		
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_4 = happyReduce 5# 6# (unsafeCoerce# reduction) where {
  reduction
	(happy_x_5 :
	happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	_ = (unsafeCoerce# happy_x_2); 
	happy_var_3 = (unsafeCoerce# happy_x_3); 
	_ = (unsafeCoerce# happy_x_4); 
	happy_var_5 = (unsafeCoerce# happy_x_5); 
	} in
		
		 ((happy_var_1,happy_var_5,Just happy_var_3)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_5 = happyReduce 6# 6# (unsafeCoerce# reduction) where {
  reduction
	(happy_x_6 :
	happy_x_5 :
	happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	_ = (unsafeCoerce# happy_x_2); 
	happy_var_3 = (unsafeCoerce# happy_x_3); 
	_ = (unsafeCoerce# happy_x_4); 
	_ = (unsafeCoerce# happy_x_5); 
	happy_var_6 = (unsafeCoerce# happy_x_6); 
	} in
		
		 ((happy_var_1,happy_var_6,Just happy_var_3)) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_6 = happySpecReduce_3 6# (unsafeCoerce# reduction) where {
  reduction
	happy_x_3
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	_ = (unsafeCoerce# happy_x_2); 
	happy_var_3 = (unsafeCoerce# happy_x_3); 
	} in
		
		 ((happy_var_1,happy_var_3,Nothing));
  reduction _ _ _  = notHappyAtAll }

happyReduce_7 = happySpecReduce_3 7# (unsafeCoerce# reduction) where {
  reduction
	happy_x_3
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	_ = (unsafeCoerce# happy_x_2); 
	happy_var_3 = (unsafeCoerce# happy_x_3); 
	} in
		
		 (happy_var_1 : happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_8 = happySpecReduce_1 7# (unsafeCoerce# reduction) where {
  reduction
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	} in
		
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_9 = happyMonadReduce 3# 8# unsafeCoerce# (unsafeCoerce# reduction) where {
  reduction
	(happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	_ = (unsafeCoerce# happy_x_3); 
	} in
		 \s l -> returnP (happy_var_1,happy_var_2,l) s l;
  reduction _ = notHappyAtAll }

happyReduce_10 = happyMonadReduce 2# 8# unsafeCoerce# (unsafeCoerce# reduction) where {
  reduction
	(happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		 \s l -> returnP (happy_var_1,happy_var_2,l) s l;
  reduction _ = notHappyAtAll }

happyReduce_11 = happySpecReduce_2 9# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (happy_var_2 : happy_var_1);
  reduction _ _  = notHappyAtAll }

happyReduce_12 = happySpecReduce_1 9# (unsafeCoerce# reduction) where {
  reduction
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	} in
		
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_13 = happySpecReduce_2 10# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	_ = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (TokenType happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_14 = happySpecReduce_2 10# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	_ = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (TokenSpec happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_15 = happySpecReduce_2 10# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	_ = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (TokenName happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_16 = happySpecReduce_3 10# (unsafeCoerce# reduction) where {
  reduction
	happy_x_3
	happy_x_2
	happy_x_1
	 =  let {
	_ = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	happy_var_3 = (unsafeCoerce# happy_x_3); 
	} in
		
		 (TokenLexer happy_var_2 happy_var_3);
  reduction _ _ _  = notHappyAtAll }

happyReduce_17 = happySpecReduce_2 10# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	_ = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (TokenMonad happy_var_2 ">>=" "return");
  reduction _ _  = notHappyAtAll }

happyReduce_18 = happyReduce 4# 10# (unsafeCoerce# reduction) where {
  reduction
	(happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	_ = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	happy_var_3 = (unsafeCoerce# happy_x_3); 
	happy_var_4 = (unsafeCoerce# happy_x_4); 
	} in
		
		 (TokenMonad happy_var_2 happy_var_3 happy_var_4) : happyRest;
  reduction _ = notHappyAtAll }

happyReduce_19 = happySpecReduce_2 11# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (happy_var_1:happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_20 = happySpecReduce_1 11# (unsafeCoerce# reduction) where {
  reduction
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	} in
		
		 ([happy_var_1]);
  reduction _  = notHappyAtAll }

happyReduce_21 = happySpecReduce_2 12# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 ((happy_var_1,happy_var_2));
  reduction _ _  = notHappyAtAll }

happyReduce_22 = happySpecReduce_2 13# (unsafeCoerce# reduction) where {
  reduction
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	happy_var_2 = (unsafeCoerce# happy_x_2); 
	} in
		
		 (happy_var_1 : happy_var_2);
  reduction _ _  = notHappyAtAll }

happyReduce_23 = happySpecReduce_0 13# (unsafeCoerce# reduction) where {
  reduction
	 =  
		 ([])}

happyReduce_24 = happySpecReduce_1 14# (unsafeCoerce# reduction) where {
  reduction
	happy_x_1
	 =  let {
	(TokenInfo happy_var_1 TokId) = (unsafeCoerce# happy_x_1); 
	} in
		
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_25 = happySpecReduce_1 15# (unsafeCoerce# reduction) where {
  reduction
	happy_x_1
	 =  let {
	happy_var_1 = (unsafeCoerce# happy_x_1); 
	} in
		
		 (Just happy_var_1);
  reduction _  = notHappyAtAll }

happyReduce_26 = happySpecReduce_0 15# (unsafeCoerce# reduction) where {
  reduction
	 =  
		 (Nothing)}

happyReduce_27 = happySpecReduce_1 16# (unsafeCoerce# reduction) where {
  reduction
	happy_x_1
	 =  let {
	(TokenInfo happy_var_1 TokCodeQuote) = (unsafeCoerce# happy_x_1); 
	} in
		
		 (happy_var_1);
  reduction _  = notHappyAtAll }

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 29# 29# (error "reading EOF!") (HappyState action) sts stk;
	TokenInfo happy_dollar_dollar TokId -> cont 17#;
	TokenKW     TokSpecId_TokenType -> cont 18#;
	TokenKW     TokSpecId_Token -> cont 19#;
	TokenKW     TokSpecId_Name -> cont 20#;
	TokenKW     TokSpecId_Lexer -> cont 21#;
	TokenKW     TokSpecId_Monad -> cont 22#;
	TokenInfo happy_dollar_dollar TokCodeQuote -> cont 23#;
	TokenKW     TokColon -> cont 24#;
	TokenKW     TokSemiColon -> cont 25#;
	TokenKW     TokDoubleColon -> cont 26#;
	TokenKW     TokDoublePercent -> cont 27#;
	TokenKW     TokBar -> cont 28#;
	})

happyThen :: P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn = returnP
ourParser = happyParse



happyError :: P a
happyError s l = failP (show l ++ ": Parse error\n") s l

-- $Id: Parser.hs,v 1.9 1999/03/11 17:16:01 simonm Exp $

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

newtype HappyState b c = HappyState
	(Int# ->			-- token number
	 Int# ->			-- token number (yes, again)
	 b -> 				-- token semantic value
	 HappyState b c ->		-- current state
	 [HappyState b c] ->		-- state stack
	 c)

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts [ ans ] = happyReturn ans
happyAccept j tk st sts _ = happyTcHack j (notHappyAtAll (-1))

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (-1#) tk st sts stk@((I# i) : _) =
--     _trace "shifting the error token" $
     new_state i i tk (HappyState new_state) (st:sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (st:sts) (unsafeCoerce# tk:stk)

-----------------------------------------------------------------------------
-- Reducing

-- happyReduce is specialised for the common cases.

-- don't allow reductions when we're in error recovery, because this can
-- lead to an infinite loop.

happySpecReduce_0 i fn (-1#) tk _ sts stk
     = case sts of
	st@(HappyState action):sts -> action (-1#) (-1#) tk st sts stk
	_ -> happyError
happySpecReduce_0 i fn j tk st@(HappyState action) sts stk
     = action i j tk st (st:sts) (fn : stk)

happySpecReduce_1 i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happySpecReduce_1 i fn j tk _ sts@(st@(HappyState action):_) (v1:stk')
     = action i j tk st sts (fn v1 : stk')
happySpecReduce_1 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_2 i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happySpecReduce_2 i fn j tk _ (_:sts@(st@(HappyState action):_)) (v1:v2:stk')
     = action i j tk st sts (fn v1 v2 : stk')
happySpecReduce_2 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_3 i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happySpecReduce_3 i fn j tk _ (_:_:sts@(st@(HappyState action):_)) 
	(v1:v2:v3:stk')
     = action i j tk st sts (fn v1 v2 v3 : stk')
happySpecReduce_3 _ _ _ _ _ _ _
     = notHappyAtAll

happyReduce k i fn (-1#) tk _ (st@(HappyState action):sts) stk
     = action (-1#) (-1#) tk st sts stk
happyReduce k i fn j tk st sts stk = action i j tk st' sts' (fn stk)
       where sts'@(st'@(HappyState action):_) = drop (I# k) (st:sts)

happyMonadReduce k i c fn (-1#) tk _ sts stk
      = case sts of
	     (st@(HappyState action):sts) -> action (-1#) (-1#) tk st sts stk
	     [] -> happyError
happyMonadReduce k i c fn j tk st sts stk =
	happyThen (fn stk) (\r -> action i j tk st' sts' (c r : stk'))
       where sts'@(st'@(HappyState action):_) = drop (I# k) (st:sts)
	     stk' = drop (I# k) stk

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto action j tk st = action j j tk (HappyState action)

-----------------------------------------------------------------------------
-- Error recovery (-1 is the error token)

-- fail if we are in recovery and no more states to discard
happyFail  (-1#) tk st' [] stk = happyError

-- discard a state
happyFail  (-1#) tk st' (st@(HappyState action):sts) stk =
--	_trace "discarding state" $
	action (-1#) (-1#) tk st sts stk

-- Enter error recovery: generate an error token,
-- 			 save the old token and carry on.
happyFail  i tk st@(HappyState action) sts stk =
--	_trace "entering error recovery" $
	action (-1#) (-1#) tk st sts ((I# i) : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

