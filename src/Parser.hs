-- parser produced by Happy Version 1.7

module Parser (ourParser,AbsSyn) where
import ParseMonad
import GenUtils
import AbsSyn
import Lexer
import Array
import Int

#ifdef __GLASGOW_HASKELL__
import GlaExts
#endif

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 (AbsSyn)
	| HappyAbsSyn5 ([(String, [([String],String,Int)], Maybe String)])
	| HappyAbsSyn6 ((String, [([String],String,Int)], Maybe String))
	| HappyAbsSyn7 ([([String],String,Int)])
	| HappyAbsSyn8 (([String],String,Int))
	| HappyAbsSyn9 ([Directive String])
	| HappyAbsSyn10 (Directive String)
	| HappyAbsSyn11 ([(String,String)])
	| HappyAbsSyn12 ((String,String))
	| HappyAbsSyn13 ([String])
	| HappyAbsSyn14 (String)
	| HappyAbsSyn15 (Maybe String)

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
 happyReduce_26,
 happyReduce_27 :: HappyReduction

action_0 (23) = happyShift action_4
action_0 (4) = happyGoto action_1
action_0 (15) = happyGoto action_2
action_0 (16) = happyGoto action_3
action_0 _ = happyReduce_26

action_1 (29) = happyAccept
action_1 _ = happyFail

action_2 (18) = happyShift action_7
action_2 (19) = happyShift action_8
action_2 (20) = happyShift action_9
action_2 (21) = happyShift action_10
action_2 (22) = happyShift action_11
action_2 (9) = happyGoto action_5
action_2 (10) = happyGoto action_6
action_2 _ = happyFail

action_3 _ = happyReduce_25

action_4 _ = happyReduce_27

action_5 (18) = happyShift action_7
action_5 (19) = happyShift action_8
action_5 (20) = happyShift action_9
action_5 (21) = happyShift action_10
action_5 (22) = happyShift action_11
action_5 (27) = happyShift action_21
action_5 (10) = happyGoto action_20
action_5 _ = happyFail

action_6 _ = happyReduce_12

action_7 (23) = happyShift action_4
action_7 (16) = happyGoto action_19
action_7 _ = happyFail

action_8 (17) = happyShift action_15
action_8 (11) = happyGoto action_16
action_8 (12) = happyGoto action_17
action_8 (14) = happyGoto action_18
action_8 _ = happyFail

action_9 (17) = happyShift action_15
action_9 (14) = happyGoto action_14
action_9 _ = happyFail

action_10 (23) = happyShift action_4
action_10 (16) = happyGoto action_13
action_10 _ = happyFail

action_11 (23) = happyShift action_4
action_11 (16) = happyGoto action_12
action_11 _ = happyFail

action_12 (23) = happyShift action_4
action_12 (16) = happyGoto action_28
action_12 _ = happyReduce_17

action_13 (23) = happyShift action_4
action_13 (16) = happyGoto action_27
action_13 _ = happyFail

action_14 _ = happyReduce_15

action_15 _ = happyReduce_24

action_16 _ = happyReduce_14

action_17 (17) = happyShift action_15
action_17 (11) = happyGoto action_26
action_17 (12) = happyGoto action_17
action_17 (14) = happyGoto action_18
action_17 _ = happyReduce_20

action_18 (23) = happyShift action_4
action_18 (16) = happyGoto action_25
action_18 _ = happyFail

action_19 _ = happyReduce_13

action_20 _ = happyReduce_11

action_21 (17) = happyShift action_15
action_21 (5) = happyGoto action_22
action_21 (6) = happyGoto action_23
action_21 (14) = happyGoto action_24
action_21 _ = happyFail

action_22 (17) = happyShift action_15
action_22 (23) = happyShift action_4
action_22 (6) = happyGoto action_32
action_22 (14) = happyGoto action_24
action_22 (15) = happyGoto action_33
action_22 (16) = happyGoto action_3
action_22 _ = happyReduce_26

action_23 _ = happyReduce_3

action_24 (24) = happyShift action_30
action_24 (26) = happyShift action_31
action_24 _ = happyFail

action_25 _ = happyReduce_21

action_26 _ = happyReduce_19

action_27 _ = happyReduce_16

action_28 (23) = happyShift action_4
action_28 (16) = happyGoto action_29
action_28 _ = happyFail

action_29 _ = happyReduce_18

action_30 (17) = happyShift action_15
action_30 (7) = happyGoto action_35
action_30 (8) = happyGoto action_36
action_30 (13) = happyGoto action_37
action_30 (14) = happyGoto action_38
action_30 _ = happyReduce_23

action_31 (23) = happyShift action_4
action_31 (16) = happyGoto action_34
action_31 _ = happyFail

action_32 _ = happyReduce_2

action_33 _ = happyReduce_1

action_34 (17) = happyShift action_15
action_34 (24) = happyShift action_43
action_34 (14) = happyGoto action_42
action_34 _ = happyFail

action_35 _ = happyReduce_6

action_36 (28) = happyShift action_41
action_36 _ = happyReduce_8

action_37 (23) = happyShift action_4
action_37 (16) = happyGoto action_40
action_37 _ = happyFail

action_38 (17) = happyShift action_15
action_38 (13) = happyGoto action_39
action_38 (14) = happyGoto action_38
action_38 _ = happyReduce_23

action_39 _ = happyReduce_22

action_40 (25) = happyShift action_47
action_40 _ = happyReduce_10

action_41 (17) = happyShift action_15
action_41 (7) = happyGoto action_46
action_41 (8) = happyGoto action_36
action_41 (13) = happyGoto action_37
action_41 (14) = happyGoto action_38
action_41 _ = happyReduce_23

action_42 (24) = happyShift action_45
action_42 _ = happyFail

action_43 (17) = happyShift action_15
action_43 (7) = happyGoto action_44
action_43 (8) = happyGoto action_36
action_43 (13) = happyGoto action_37
action_43 (14) = happyGoto action_38
action_43 _ = happyReduce_23

action_44 _ = happyReduce_4

action_45 (17) = happyShift action_15
action_45 (7) = happyGoto action_48
action_45 (8) = happyGoto action_36
action_45 (13) = happyGoto action_37
action_45 (14) = happyGoto action_38
action_45 _ = happyReduce_23

action_46 _ = happyReduce_7

action_47 _ = happyReduce_9

action_48 _ = happyReduce_5

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn15  happy_var_5) :
	(HappyAbsSyn5  happy_var_4) :
	_ :
	(HappyAbsSyn9  happy_var_2) :
	(HappyAbsSyn15  happy_var_1) :
	happyRest)
	 = HappyAbsSyn4
		 (AbsSyn happy_var_1 (reverse happy_var_2) (reverse happy_var_4) happy_var_5
	) : happyRest
happyReduction_1 _ = notHappyAtAll 

happyReduce_2 = happySpecReduce_2 5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1 5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn7  happy_var_5) :
	_ :
	(HappyAbsSyn14  happy_var_3) :
	_ :
	(HappyAbsSyn14  happy_var_1) :
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1,happy_var_5,Just happy_var_3)
	) : happyRest
happyReduction_4 _ = notHappyAtAll 

happyReduce_5 = happyReduce 6 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn7  happy_var_6) :
	_ :
	_ :
	(HappyAbsSyn14  happy_var_3) :
	_ :
	(HappyAbsSyn14  happy_var_1) :
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1,happy_var_6,Just happy_var_3)
	) : happyRest
happyReduction_5 _ = notHappyAtAll 

happyReduce_6 = happySpecReduce_3 6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 ((happy_var_1,happy_var_3,Nothing)
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3 7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1 7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyMonadReduce 3 8 happyReduction_9
happyReduction_9 (_ :
	(HappyAbsSyn14  happy_var_2) :
	(HappyAbsSyn13  happy_var_1) :
	happyRest)
	 = happyThen ( \s l -> returnP (happy_var_1,happy_var_2,l) s l
	) (\r -> happyReturn (HappyAbsSyn8 r))
happyReduction_9 _ = notHappyAtAll 

happyReduce_10 = happyMonadReduce 2 8 happyReduction_10
happyReduction_10 ((HappyAbsSyn14  happy_var_2) :
	(HappyAbsSyn13  happy_var_1) :
	happyRest)
	 = happyThen ( \s l -> returnP (happy_var_1,happy_var_2,l) s l
	) (\r -> happyReturn (HappyAbsSyn8 r))
happyReduction_10 _ = notHappyAtAll 

happyReduce_11 = happySpecReduce_2 9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1 9 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2 10 happyReduction_13
happyReduction_13 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TokenType happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2 10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TokenSpec happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2 10 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TokenName happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3 10 happyReduction_16
happyReduction_16 (HappyAbsSyn14  happy_var_3)
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TokenLexer happy_var_2 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2 10 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TokenMonad happy_var_2 ">>=" "return"
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn14  happy_var_4) :
	(HappyAbsSyn14  happy_var_3) :
	(HappyAbsSyn14  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn10
		 (TokenMonad happy_var_2 happy_var_3 happy_var_4
	) : happyRest
happyReduction_18 _ = notHappyAtAll 

happyReduce_19 = happySpecReduce_2 11 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1:happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1 11 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2 12 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 ((happy_var_1,happy_var_2)
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2 13 happyReduction_22
happyReduction_22 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0 13 happyReduction_23
happyReduction_23  =  HappyAbsSyn13
		 ([]
	)

happyReduce_24 = happySpecReduce_1 14 happyReduction_24
happyReduction_24 (HappyTerminal (TokenInfo happy_var_1 TokId))
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1 15 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (Just happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0 15 happyReduction_26
happyReduction_26  =  HappyAbsSyn15
		 (Nothing
	)

happyReduce_27 = happySpecReduce_1 16 happyReduction_27
happyReduction_27 (HappyTerminal (TokenInfo happy_var_1 TokCodeQuote))
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 29 29 (error "reading EOF!") (HappyState action) sts stk;
	TokenInfo happy_dollar_dollar TokId -> cont 17;
	TokenKW     TokSpecId_TokenType -> cont 18;
	TokenKW     TokSpecId_Token -> cont 19;
	TokenKW     TokSpecId_Name -> cont 20;
	TokenKW     TokSpecId_Lexer -> cont 21;
	TokenKW     TokSpecId_Monad -> cont 22;
	TokenInfo happy_dollar_dollar TokCodeQuote -> cont 23;
	TokenKW     TokColon -> cont 24;
	TokenKW     TokSemiColon -> cont 25;
	TokenKW     TokDoubleColon -> cont 26;
	TokenKW     TokDoublePercent -> cont 27;
	TokenKW     TokBar -> cont 28;
	})

happyThen :: P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn = (returnP)
ourParser = happyParse

happyError :: P a
happyError s l = failP (show l ++ ": Parse error\n") s l{-# LINE 1 "GenericTemplate.hs" -}
{-# LINE 1 "GenericTemplate.hs" -}
-- $Id: Parser.hs,v 1.12 1999/12/06 12:27:18 panne Exp $

{-# LINE 15 "GenericTemplate.hs" -}




























































-----------------------------------------------------------------------------
-- starting the parse

happyParse = happyNewToken action_0 [] []

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 123 "GenericTemplate.hs" -}


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts [ ans ] = happyReturn (case (ans) of {HappyAbsSyn4 z -> z })
happyAccept j tk st sts _       = 

				  notHappyAtAll

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x : _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk)):stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn : stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1:stk')
     = action nt j tk st sts (fn v1 : stk')
happySpecReduce_1 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1:v2:stk')
     = action nt j tk st sts (fn v1 v2 : stk')
happySpecReduce_2 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1:v2:v3:stk')
     = action nt j tk st sts (fn v1 v2 v3 : stk')
happySpecReduce_3 _ _ _ _ _ _ _
     = notHappyAtAll

happyRedcue k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk = action nt j tk st1 sts1 (fn stk)
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen (fn stk) (\r -> action nt j tk st1 sts1 (r : drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = drop (k) stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - (1)) t

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 214 "GenericTemplate.hs" -}

happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st [] stk =
--	trace "failing" $ 
    	happyError

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok : _ : stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok:stk))

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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
