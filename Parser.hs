-- parser produced by Happy Version 0.8


module Parser (ourParser,AbsSyn) where
import GenUtils
import AbsSyn
import Lexer


data HappyAbsSyn 
	= HappyTerminal ( Token )
	| HappyAbsSyn1 (  AbsSyn  )
	| HappyAbsSyn2 (  [(String, [([String],String,Int)], Maybe String)]  )
	| HappyAbsSyn3 (  (String, [([String],String,Int)], Maybe String)  )
	| HappyAbsSyn4 (  [([String],String,Int)]  )
	| HappyAbsSyn5 (  ([String],String,Int)  )
	| HappyAbsSyn6 (  [Directive String]  )
	| HappyAbsSyn7 (  Directive String  )
	| HappyAbsSyn8 (  [(String,String)]  )
	| HappyAbsSyn9 (  (String,String)  )
	| HappyAbsSyn10 (  [String]  )
	| HappyAbsSyn11 (  String  )
	| HappyAbsSyn12 (  Maybe String  )
	| HappyAbsSyn13 (  String  )

type HappyReduction = 
	   Int 
	-> ( Token ) 
	-> HappyState ( Token ) ([HappyAbsSyn] -> ( AbsSyn )) 
	-> Int 
	-> [ Token ] 
	-> [HappyState ( Token ) ([HappyAbsSyn] -> ( AbsSyn ))] 
	-> [HappyAbsSyn] 
	-> ( AbsSyn )

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
 action_41 :: Int -> HappyReduction

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
 happyReduce_24 :: HappyReduction

action_0 19 = happyShift action_4
action_0 1 = happyGoto action_1
action_0 12 = happyGoto action_2
action_0 13 = happyGoto action_3
action_0 _ = happyReduce_23

action_1 25 = happyAccept
action_1 _ = happyFail

action_2 15 = happyShift action_7
action_2 16 = happyShift action_8
action_2 17 = happyShift action_9
action_2 18 = happyShift action_10
action_2 6 = happyGoto action_5
action_2 7 = happyGoto action_6
action_2 _ = happyFail

action_3 _ = happyReduce_22

action_4 _ = happyReduce_24

action_5 15 = happyShift action_7
action_5 16 = happyShift action_8
action_5 17 = happyShift action_9
action_5 18 = happyShift action_10
action_5 23 = happyShift action_19
action_5 7 = happyGoto action_18
action_5 _ = happyFail

action_6 _ = happyReduce_11

action_7 19 = happyShift action_4
action_7 13 = happyGoto action_17
action_7 _ = happyFail

action_8 14 = happyShift action_13
action_8 8 = happyGoto action_14
action_8 9 = happyGoto action_15
action_8 11 = happyGoto action_16
action_8 _ = happyFail

action_9 14 = happyShift action_13
action_9 11 = happyGoto action_12
action_9 _ = happyFail

action_10 19 = happyShift action_4
action_10 13 = happyGoto action_11
action_10 _ = happyFail

action_11 _ = happyReduce_15

action_12 _ = happyReduce_14

action_13 _ = happyReduce_21

action_14 _ = happyReduce_13

action_15 14 = happyShift action_13
action_15 8 = happyGoto action_24
action_15 9 = happyGoto action_15
action_15 11 = happyGoto action_16
action_15 _ = happyReduce_17

action_16 19 = happyShift action_4
action_16 13 = happyGoto action_23
action_16 _ = happyFail

action_17 _ = happyReduce_12

action_18 _ = happyReduce_10

action_19 14 = happyShift action_13
action_19 2 = happyGoto action_20
action_19 3 = happyGoto action_21
action_19 11 = happyGoto action_22
action_19 _ = happyFail

action_20 14 = happyShift action_13
action_20 19 = happyShift action_4
action_20 3 = happyGoto action_27
action_20 11 = happyGoto action_22
action_20 12 = happyGoto action_28
action_20 13 = happyGoto action_3
action_20 _ = happyReduce_23

action_21 _ = happyReduce_3

action_22 20 = happyShift action_25
action_22 22 = happyShift action_26
action_22 _ = happyFail

action_23 _ = happyReduce_18

action_24 _ = happyReduce_16

action_25 14 = happyShift action_13
action_25 4 = happyGoto action_30
action_25 5 = happyGoto action_31
action_25 10 = happyGoto action_32
action_25 11 = happyGoto action_33
action_25 _ = happyReduce_20

action_26 19 = happyShift action_4
action_26 13 = happyGoto action_29
action_26 _ = happyFail

action_27 _ = happyReduce_2

action_28 _ = happyReduce_1

action_29 14 = happyShift action_13
action_29 11 = happyGoto action_37
action_29 _ = happyFail

action_30 _ = happyReduce_5

action_31 24 = happyShift action_36
action_31 _ = happyReduce_7

action_32 19 = happyShift action_4
action_32 13 = happyGoto action_35
action_32 _ = happyFail

action_33 14 = happyShift action_13
action_33 10 = happyGoto action_34
action_33 11 = happyGoto action_33
action_33 _ = happyReduce_20

action_34 _ = happyReduce_19

action_35 21 = happyShift action_40
action_35 _ = happyReduce_9

action_36 14 = happyShift action_13
action_36 4 = happyGoto action_39
action_36 5 = happyGoto action_31
action_36 10 = happyGoto action_32
action_36 11 = happyGoto action_33
action_36 _ = happyReduce_20

action_37 20 = happyShift action_38
action_37 _ = happyFail

action_38 14 = happyShift action_13
action_38 4 = happyGoto action_41
action_38 5 = happyGoto action_31
action_38 10 = happyGoto action_32
action_38 11 = happyGoto action_33
action_38 _ = happyReduce_20

action_39 _ = happyReduce_6

action_40 _ = happyReduce_8

action_41 _ = happyReduce_4

happyReduce_1 = happyReduce 1 5 reduction where {
  reduction (
	HappyAbsSyn12  happy_var_5 :
	HappyAbsSyn2  happy_var_4 :
	_ :
	HappyAbsSyn6  happy_var_2 :
	HappyAbsSyn12  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn1
		(  AbsSyn happy_var_1 (reverse happy_var_2) (reverse happy_var_4) happy_var_5  ) : happyRest;
  reduction _ _ = notHappyAtAll 1}

happyReduce_2 = specHappyReduce_2 2 reduction where {
  reduction (
	HappyAbsSyn3  happy_var_2 :
	HappyAbsSyn2  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn2
		(  happy_var_2 : happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 2}

happyReduce_3 = specHappyReduce_1 2 reduction where {
  reduction (
	HappyAbsSyn3  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn2
		(  [happy_var_1]  ) : happyRest;
  reduction _ _ = notHappyAtAll 3}

happyReduce_4 = happyReduce 3 6 reduction where {
  reduction (
	HappyAbsSyn4  happy_var_6 :
	_ :
	_ :
	HappyAbsSyn13  happy_var_3 :
	_ :
	HappyAbsSyn11  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn3
		(  (happy_var_1,happy_var_6,Just happy_var_3)  ) : happyRest;
  reduction _ _ = notHappyAtAll 4}

happyReduce_5 = specHappyReduce_3 3 reduction where {
  reduction (
	HappyAbsSyn4  happy_var_3 :
	_ :
	HappyAbsSyn11  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn3
		(  (happy_var_1,happy_var_3,Nothing)  ) : happyRest;
  reduction _ _ = notHappyAtAll 5}

happyReduce_6 = specHappyReduce_3 4 reduction where {
  reduction (
	HappyAbsSyn4  happy_var_3 :
	_ :
	HappyAbsSyn5  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn4
		(  happy_var_1 : happy_var_3  ) : happyRest;
  reduction _ _ = notHappyAtAll 6}

happyReduce_7 = specHappyReduce_1 4 reduction where {
  reduction (
	HappyAbsSyn5  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn4
		(  [happy_var_1]  ) : happyRest;
  reduction _ _ = notHappyAtAll 7}

happyReduce_8 = specHappyReduce_3 5 reduction where {
  reduction (
	_ :
	HappyAbsSyn13  happy_var_2 :
	HappyAbsSyn10  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn5
		(  (happy_var_1,happy_var_2,happy_var_lineno)  ) : happyRest;
  reduction _ _ = notHappyAtAll 8}

happyReduce_9 = specHappyReduce_2 5 reduction where {
  reduction (
	HappyAbsSyn13  happy_var_2 :
	HappyAbsSyn10  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn5
		(  (happy_var_1,happy_var_2,happy_var_lineno)  ) : happyRest;
  reduction _ _ = notHappyAtAll 9}

happyReduce_10 = specHappyReduce_2 6 reduction where {
  reduction (
	HappyAbsSyn7  happy_var_2 :
	HappyAbsSyn6  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn6
		(  happy_var_2 : happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 10}

happyReduce_11 = specHappyReduce_1 6 reduction where {
  reduction (
	HappyAbsSyn7  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn6
		(  [happy_var_1]  ) : happyRest;
  reduction _ _ = notHappyAtAll 11}

happyReduce_12 = specHappyReduce_2 7 reduction where {
  reduction (
	HappyAbsSyn13  happy_var_2 :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn7
		(  TokenType happy_var_2  ) : happyRest;
  reduction _ _ = notHappyAtAll 12}

happyReduce_13 = specHappyReduce_2 7 reduction where {
  reduction (
	HappyAbsSyn8  happy_var_2 :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn7
		(  TokenSpec happy_var_2  ) : happyRest;
  reduction _ _ = notHappyAtAll 13}

happyReduce_14 = specHappyReduce_2 7 reduction where {
  reduction (
	HappyAbsSyn11  happy_var_2 :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn7
		(  TokenName happy_var_2  ) : happyRest;
  reduction _ _ = notHappyAtAll 14}

happyReduce_15 = specHappyReduce_2 7 reduction where {
  reduction (
	HappyAbsSyn13  happy_var_2 :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn7
		(  TokenNewline happy_var_2  ) : happyRest;
  reduction _ _ = notHappyAtAll 15}

happyReduce_16 = specHappyReduce_2 8 reduction where {
  reduction (
	HappyAbsSyn8  happy_var_2 :
	HappyAbsSyn9  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn8
		(  happy_var_1:happy_var_2  ) : happyRest;
  reduction _ _ = notHappyAtAll 16}

happyReduce_17 = specHappyReduce_1 8 reduction where {
  reduction (
	HappyAbsSyn9  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn8
		(  [happy_var_1]  ) : happyRest;
  reduction _ _ = notHappyAtAll 17}

happyReduce_18 = specHappyReduce_2 9 reduction where {
  reduction (
	HappyAbsSyn13  happy_var_2 :
	HappyAbsSyn11  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn9
		(  (happy_var_1,happy_var_2)  ) : happyRest;
  reduction _ _ = notHappyAtAll 18}

happyReduce_19 = specHappyReduce_2 10 reduction where {
  reduction (
	HappyAbsSyn10  happy_var_2 :
	HappyAbsSyn11  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn10
		(  happy_var_1 : happy_var_2  ) : happyRest;
  reduction _ _ = notHappyAtAll 19}

happyReduce_20 = specHappyReduce_0 10 reduction where {
  reduction (
	happyRest)
	happy_var_lineno = HappyAbsSyn10
		(  []  ) : happyRest}

happyReduce_21 = specHappyReduce_1 11 reduction where {
  reduction (
	HappyTerminal ( TokenInfo happy_var_1 TokId ) :
	happyRest)
	happy_var_lineno = HappyAbsSyn11
		(  happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 21}

happyReduce_22 = specHappyReduce_1 12 reduction where {
  reduction (
	HappyAbsSyn13  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn12
		(  Just happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 22}

happyReduce_23 = specHappyReduce_0 12 reduction where {
  reduction (
	happyRest)
	happy_var_lineno = HappyAbsSyn12
		(  Nothing  ) : happyRest}

happyReduce_24 = specHappyReduce_1 13 reduction where {
  reduction (
	HappyTerminal ( TokenInfo happy_var_1 TokCodeQuote ) :
	happyRest)
	happy_var_lineno = HappyAbsSyn13
		(  happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 24}

happyNewToken action ln []
	= action 25 25 (error "reading EOF!") (HappyState action) ln []

happyNewToken action ln (tk:tks) = case tk of
	 TokenInfo _ TokId  -> cont 14
	 TokenKW     TokSpecId_TokenType  -> cont 15
	 TokenKW     TokSpecId_Token  -> cont 16
	 TokenKW     TokSpecId_Name  -> cont 17
	 TokenKW     TokSpecId_Newline  -> cont 18
	 TokenInfo _ TokCodeQuote  -> cont 19
	 TokenKW     TokColon  -> cont 20
	 TokenKW     TokSemiColon  -> cont 21
	 TokenKW     TokDoubleColon  -> cont 22
	 TokenKW     TokDoublePercent  -> cont 23
	 TokenKW     TokBar  -> cont 24
	 TokenKW     TokNewLine  -> happyNewToken action (ln + 1) tks
  where cont i = action i i tk (HappyState action) ln tks

ourParser = happyParse



happyError :: Int -> [Token] -> a
happyError i (t:ts) = 
	error ("Parse error in line " ++ show i ++ "\n")

-- Start of Happy Template (version 0.8)

happyParse tks = happyNewToken action_0 (1::Int) tks [] []

-- All this HappyState stuff is simply because we can't have recursive
-- types in Haskell without an intervening data structure.

data HappyState b c = HappyState
        (Int ->                         -- token number
         Int ->                         -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         Int ->                         -- line number
         [b] ->                         -- rest of tokens
         [HappyState b c] ->            -- state stack
         c)

-- Ok, Here are the action functions.

happyAccept _ _ _ _ _ _ [ HappyAbsSyn1 ans ] = ans

happyFail   _ _ _ ln tks _ _ = happyError ln tks

happyShift new_state i tk st ln tks sts stk =
     happyNewToken new_state ln tks (st:sts) (HappyTerminal tk:stk)

happyGoto action j tk st = action j j tk (HappyState action)

-- happyReduce is specialised for the common cases.

specHappyReduce_0 i fn j tk st@(HappyState action) ln tks sts stk
     = action i j tk st ln tks (st:sts) (fn stk ln)
specHappyReduce_1 i fn j tk _ ln tks sts@(st@(HappyState action):_) stk
     = action i j tk st ln tks sts (fn stk ln)
specHappyReduce_2 i fn j tk _ ln tks (_:sts@(st@(HappyState action):_)) stk
     = action i j tk st ln tks sts (fn stk ln)
specHappyReduce_3 i fn j tk _ ln tks (_:_:sts@(st@(HappyState action):_)) stk
     = action i j tk st ln tks sts (fn stk ln)

happyReduce i k fn j tk st ln tks sts stk
              = action i j tk st' ln tks sts' (fn stk ln)
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)

-- Internal happy errors:

notHappyAtAll :: Int -> a
notHappyAtAll i = error ("Internal Happy error in reduction ( " 
                           ++ show i ++ " )")

-- end of Happy Template.

