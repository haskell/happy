-- parser produced by Happy Version 1.8

module Parser (ourParser,AbsSyn) where
import ParseMonad
import GenUtils
import AbsSyn
import Lexer

#ifdef __GLASGOW_HASKELL__
import GlaExts
#endif

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 (AbsSyn)
	| HappyAbsSyn5 ([(String, [([String],String,Int,Maybe String)], Maybe String)])
	| HappyAbsSyn6 ((String, [([String],String,Int,Maybe String)], Maybe String))
	| HappyAbsSyn7 ([([String],String,Int,Maybe String)])
	| HappyAbsSyn8 (([String],String,Int,Maybe String))
	| HappyAbsSyn9 (Maybe String)
	| HappyAbsSyn10 ([Directive String])
	| HappyAbsSyn11 (Directive String)
	| HappyAbsSyn12 ([(String,String)])
	| HappyAbsSyn13 ((String,String))
	| HappyAbsSyn14 ([String])
	| HappyAbsSyn15 (String)

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
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57 :: Int -> HappyReduction

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
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32 :: HappyReduction

action_0 (28) = happyShift action_4
action_0 (4) = happyGoto action_1
action_0 (16) = happyGoto action_2
action_0 (17) = happyGoto action_3
action_0 _ = happyReduce_31

action_1 (34) = happyAccept
action_1 _ = happyFail

action_2 (19) = happyShift action_7
action_2 (20) = happyShift action_8
action_2 (21) = happyShift action_9
action_2 (22) = happyShift action_10
action_2 (23) = happyShift action_11
action_2 (24) = happyShift action_12
action_2 (25) = happyShift action_13
action_2 (26) = happyShift action_14
action_2 (10) = happyGoto action_5
action_2 (11) = happyGoto action_6
action_2 _ = happyFail

action_3 _ = happyReduce_30

action_4 _ = happyReduce_32

action_5 (19) = happyShift action_7
action_5 (20) = happyShift action_8
action_5 (21) = happyShift action_9
action_5 (22) = happyShift action_10
action_5 (23) = happyShift action_11
action_5 (24) = happyShift action_12
action_5 (25) = happyShift action_13
action_5 (26) = happyShift action_14
action_5 (32) = happyShift action_28
action_5 (11) = happyGoto action_27
action_5 _ = happyFail

action_6 _ = happyReduce_14

action_7 (28) = happyShift action_4
action_7 (17) = happyGoto action_26
action_7 _ = happyFail

action_8 (18) = happyShift action_17
action_8 (12) = happyGoto action_23
action_8 (13) = happyGoto action_24
action_8 (15) = happyGoto action_25
action_8 _ = happyFail

action_9 (18) = happyShift action_17
action_9 (15) = happyGoto action_22
action_9 _ = happyFail

action_10 (28) = happyShift action_4
action_10 (17) = happyGoto action_21
action_10 _ = happyFail

action_11 (28) = happyShift action_4
action_11 (17) = happyGoto action_20
action_11 _ = happyFail

action_12 (18) = happyShift action_17
action_12 (14) = happyGoto action_19
action_12 (15) = happyGoto action_16
action_12 _ = happyReduce_28

action_13 (18) = happyShift action_17
action_13 (14) = happyGoto action_18
action_13 (15) = happyGoto action_16
action_13 _ = happyReduce_28

action_14 (18) = happyShift action_17
action_14 (14) = happyGoto action_15
action_14 (15) = happyGoto action_16
action_14 _ = happyReduce_28

action_15 _ = happyReduce_22

action_16 (18) = happyShift action_17
action_16 (14) = happyGoto action_36
action_16 (15) = happyGoto action_16
action_16 _ = happyReduce_28

action_17 _ = happyReduce_29

action_18 _ = happyReduce_23

action_19 _ = happyReduce_21

action_20 (28) = happyShift action_4
action_20 (17) = happyGoto action_35
action_20 _ = happyReduce_19

action_21 (28) = happyShift action_4
action_21 (17) = happyGoto action_34
action_21 _ = happyFail

action_22 _ = happyReduce_17

action_23 _ = happyReduce_16

action_24 (18) = happyShift action_17
action_24 (12) = happyGoto action_33
action_24 (13) = happyGoto action_24
action_24 (15) = happyGoto action_25
action_24 _ = happyReduce_25

action_25 (28) = happyShift action_4
action_25 (17) = happyGoto action_32
action_25 _ = happyFail

action_26 _ = happyReduce_15

action_27 _ = happyReduce_13

action_28 (18) = happyShift action_17
action_28 (5) = happyGoto action_29
action_28 (6) = happyGoto action_30
action_28 (15) = happyGoto action_31
action_28 _ = happyFail

action_29 (18) = happyShift action_17
action_29 (28) = happyShift action_4
action_29 (6) = happyGoto action_40
action_29 (15) = happyGoto action_31
action_29 (16) = happyGoto action_41
action_29 (17) = happyGoto action_3
action_29 _ = happyReduce_31

action_30 _ = happyReduce_3

action_31 (29) = happyShift action_38
action_31 (31) = happyShift action_39
action_31 _ = happyFail

action_32 _ = happyReduce_26

action_33 _ = happyReduce_24

action_34 _ = happyReduce_18

action_35 (28) = happyShift action_4
action_35 (17) = happyGoto action_37
action_35 _ = happyFail

action_36 _ = happyReduce_27

action_37 _ = happyReduce_20

action_38 (18) = happyShift action_17
action_38 (7) = happyGoto action_43
action_38 (8) = happyGoto action_44
action_38 (14) = happyGoto action_45
action_38 (15) = happyGoto action_16
action_38 _ = happyReduce_28

action_39 (28) = happyShift action_4
action_39 (17) = happyGoto action_42
action_39 _ = happyFail

action_40 _ = happyReduce_2

action_41 _ = happyReduce_1

action_42 (18) = happyShift action_17
action_42 (29) = happyShift action_50
action_42 (15) = happyGoto action_49
action_42 _ = happyFail

action_43 _ = happyReduce_6

action_44 (33) = happyShift action_48
action_44 _ = happyReduce_8

action_45 (27) = happyShift action_47
action_45 (9) = happyGoto action_46
action_45 _ = happyReduce_12

action_46 (28) = happyShift action_4
action_46 (17) = happyGoto action_55
action_46 _ = happyFail

action_47 (18) = happyShift action_17
action_47 (15) = happyGoto action_54
action_47 _ = happyFail

action_48 (18) = happyShift action_17
action_48 (7) = happyGoto action_53
action_48 (8) = happyGoto action_44
action_48 (14) = happyGoto action_45
action_48 (15) = happyGoto action_16
action_48 _ = happyReduce_28

action_49 (29) = happyShift action_52
action_49 _ = happyFail

action_50 (18) = happyShift action_17
action_50 (7) = happyGoto action_51
action_50 (8) = happyGoto action_44
action_50 (14) = happyGoto action_45
action_50 (15) = happyGoto action_16
action_50 _ = happyReduce_28

action_51 _ = happyReduce_4

action_52 (18) = happyShift action_17
action_52 (7) = happyGoto action_57
action_52 (8) = happyGoto action_44
action_52 (14) = happyGoto action_45
action_52 (15) = happyGoto action_16
action_52 _ = happyReduce_28

action_53 _ = happyReduce_7

action_54 _ = happyReduce_11

action_55 (30) = happyShift action_56
action_55 _ = happyReduce_10

action_56 _ = happyReduce_9

action_57 _ = happyReduce_5

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn9  happy_var_5) :
	(HappyAbsSyn5  happy_var_4) :
	_ :
	(HappyAbsSyn10  happy_var_2) :
	(HappyAbsSyn9  happy_var_1) :
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
	(HappyAbsSyn15  happy_var_3) :
	_ :
	(HappyAbsSyn15  happy_var_1) :
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1,happy_var_5,Just happy_var_3)
	) : happyRest
happyReduction_4 _ = notHappyAtAll 

happyReduce_5 = happyReduce 6 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn7  happy_var_6) :
	_ :
	_ :
	(HappyAbsSyn15  happy_var_3) :
	_ :
	(HappyAbsSyn15  happy_var_1) :
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1,happy_var_6,Just happy_var_3)
	) : happyRest
happyReduction_5 _ = notHappyAtAll 

happyReduce_6 = happySpecReduce_3 6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
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

happyReduce_9 = happyMonadReduce 4 8 happyReduction_9
happyReduction_9 (_ :
	(HappyAbsSyn15  happy_var_3) :
	(HappyAbsSyn9  happy_var_2) :
	(HappyAbsSyn14  happy_var_1) :
	happyRest)
	 = happyThen ( \s l -> returnP (happy_var_1,happy_var_3,l,happy_var_2) s l
	) (\r -> happyReturn (HappyAbsSyn8 r))
happyReduction_9 _ = notHappyAtAll 

happyReduce_10 = happyMonadReduce 3 8 happyReduction_10
happyReduction_10 ((HappyAbsSyn15  happy_var_3) :
	(HappyAbsSyn9  happy_var_2) :
	(HappyAbsSyn14  happy_var_1) :
	happyRest)
	 = happyThen ( \s l -> returnP (happy_var_1,happy_var_3,l,happy_var_2) s l
	) (\r -> happyReturn (HappyAbsSyn8 r))
happyReduction_10 _ = notHappyAtAll 

happyReduce_11 = happySpecReduce_2 9 happyReduction_11
happyReduction_11 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Just happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0 9 happyReduction_12
happyReduction_12  =  HappyAbsSyn9
		 (Nothing
	)

happyReduce_13 = happySpecReduce_2 10 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1 10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2 11 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenType happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2 11 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenSpec happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2 11 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenName happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3 11 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenLexer happy_var_2 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2 11 happyReduction_19
happyReduction_19 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenMonad happy_var_2 ">>=" "return"
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 11 happyReduction_20
happyReduction_20 ((HappyAbsSyn15  happy_var_4) :
	(HappyAbsSyn15  happy_var_3) :
	(HappyAbsSyn15  happy_var_2) :
	_ :
	happyRest)
	 = HappyAbsSyn11
		 (TokenMonad happy_var_2 happy_var_3 happy_var_4
	) : happyRest
happyReduction_20 _ = notHappyAtAll 

happyReduce_21 = happySpecReduce_2 11 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenNonassoc happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2 11 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenRight happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2 11 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TokenLeft happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2 12 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1:happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1 12 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2 13 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn13
		 ((happy_var_1,happy_var_2)
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2 14 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0 14 happyReduction_28
happyReduction_28  =  HappyAbsSyn14
		 ([]
	)

happyReduce_29 = happySpecReduce_1 15 happyReduction_29
happyReduction_29 (HappyTerminal (TokenInfo happy_var_1 TokId))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1 16 happyReduction_30
happyReduction_30 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn9
		 (Just happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0 16 happyReduction_31
happyReduction_31  =  HappyAbsSyn9
		 (Nothing
	)

happyReduce_32 = happySpecReduce_1 17 happyReduction_32
happyReduction_32 (HappyTerminal (TokenInfo happy_var_1 TokCodeQuote))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 34 34 (error "reading EOF!") (HappyState action) sts stk;
	TokenInfo happy_dollar_dollar TokId -> cont 18;
	TokenKW     TokSpecId_TokenType -> cont 19;
	TokenKW     TokSpecId_Token -> cont 20;
	TokenKW     TokSpecId_Name -> cont 21;
	TokenKW     TokSpecId_Lexer -> cont 22;
	TokenKW     TokSpecId_Monad -> cont 23;
	TokenKW     TokSpecId_Nonassoc -> cont 24;
	TokenKW     TokSpecId_Left -> cont 25;
	TokenKW     TokSpecId_Right -> cont 26;
	TokenKW     TokSpecId_Prec -> cont 27;
	TokenInfo happy_dollar_dollar TokCodeQuote -> cont 28;
	TokenKW     TokColon -> cont 29;
	TokenKW     TokSemiColon -> cont 30;
	TokenKW     TokDoubleColon -> cont 31;
	TokenKW     TokDoublePercent -> cont 32;
	TokenKW     TokBar -> cont 33;
	})

happyThen :: P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 = happyReturn

ourParser = happyParse

happyError :: P a
happyError s l = failP (show l ++ ": Parse error\n") s l{-# LINE 1 "GenericTemplate.hs" -}
{-# LINE 1 "GenericTemplate.hs" -}
-- $Id: Parser.hs,v 1.14 2000/07/13 14:25:43 simonmar Exp $

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

happyAccept j tk st sts [ ans ] = happyReturn1 (case (ans) of {HappyAbsSyn4 z -> z })
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
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r : drop_stk))
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
