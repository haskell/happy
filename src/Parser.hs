{-# OPTIONS -fglasgow-exts #-}
-- parser produced by Happy Version 1.7

module Parser (ourParser,AbsSyn) where
import ParseMonad
import GenUtils
import AbsSyn
import Lexer

#ifdef __GLASGOW_HASKELL__
import GlaExts
#endif
import GlaExts
import Array

type HappyAbsSyn  = ()
happyIn4 :: (AbsSyn) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (AbsSyn)
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([(String, [([String],String,Int,Maybe String)], Maybe String)]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([(String, [([String],String,Int,Maybe String)], Maybe String)])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ((String, [([String],String,Int,Maybe String)], Maybe String)) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ((String, [([String],String,Int,Maybe String)], Maybe String))
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ([([String],String,Int,Maybe String)]) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ([([String],String,Int,Maybe String)])
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (([String],String,Int,Maybe String)) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (([String],String,Int,Maybe String))
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Maybe String) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Maybe String)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([Directive String]) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([Directive String])
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Directive String) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Directive String)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([(String,String)]) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([(String,String)])
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ((String,String)) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ((String,String))
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([String]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([String])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (String) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (String)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Maybe String) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Maybe String)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (String) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (String)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: Addr
happyActOffsets = A# "\x20\x00\x1b\x00\x14\x00\x00\x00\x00\x00\xff\xff\x00\x00\x1f\x00\x27\x00\x27\x00\x1c\x00\x1c\x00\x25\x00\x25\x00\x25\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x1a\x00\x00\x00\x00\x00\x23\x00\x18\x00\x00\x00\x00\x00\x21\x00\x0a\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x1e\x00\x15\x00\x00\x00\x00\x00\x08\x00\x00\x00\x0e\x00\x09\x00\x07\x00\x10\x00\x10\x00\x04\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: Addr
happyGotoOffsets = A# "\x29\x00\x00\x00\x55\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x57\x00\x49\x00\x43\x00\x56\x00\x54\x00\x4f\x00\x4d\x00\x4b\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x53\x00\x52\x00\x00\x00\x00\x00\x42\x00\x51\x00\x00\x00\x00\x00\x2f\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x45\x00\x46\x00\x00\x00\x00\x00\x41\x00\x00\x00\x00\x00\x2a\x00\x34\x00\x22\x00\x3c\x00\x00\x00\x3a\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: Addr
happyDefActions = A# "\xe0\xff\x00\x00\x00\x00\xe1\xff\xdf\xff\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\xe3\xff\xe3\xff\xe9\xff\xe3\xff\xe2\xff\xe8\xff\xea\xff\xec\xff\x00\x00\xee\xff\xef\xff\xe6\xff\x00\x00\xf0\xff\xf2\xff\x00\x00\xe0\xff\xfc\xff\x00\x00\xe5\xff\xe7\xff\xed\xff\x00\x00\xe4\xff\xeb\xff\xe3\xff\x00\x00\xfd\xff\xfe\xff\x00\x00\xf9\xff\xf7\xff\xf3\xff\x00\x00\x00\x00\xe3\xff\x00\x00\xe3\xff\xfb\xff\xe3\xff\xf8\xff\xf4\xff\xf5\xff\xf6\xff\xfa\xff"#

happyCheck :: Addr
happyCheck = A# "\xff\xff\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x01\x00\x0c\x00\x01\x00\x0e\x00\x01\x00\x0f\x00\x0d\x00\x0c\x00\x01\x00\x0b\x00\x0a\x00\x0c\x00\x0b\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x10\x00\x01\x00\x0b\x00\x0b\x00\x01\x00\x0b\x00\x01\x00\x0b\x00\x01\x00\x0b\x00\x01\x00\x00\x00\x0b\x00\x0b\x00\x11\x00\x0b\x00\x02\x00\x05\x00\x01\x00\x02\x00\x0a\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x04\x00\x0d\x00\x0a\x00\x0b\x00\x0a\x00\x0b\x00\x0a\x00\x0b\x00\x03\x00\x04\x00\x08\x00\x09\x00\x0b\x00\x0b\x00\x0b\x00\x0a\x00\x0b\x00\x08\x00\x09\x00\x0d\x00\x0b\x00\x0a\x00\x0b\x00\x0a\x00\x0b\x00\x0a\x00\x0b\x00\x06\x00\x07\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\xff\xff\x0d\x00\x0d\x00"#

happyTable :: Addr
happyTable = A# "\x00\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x12\x00\x27\x00\x12\x00\x28\x00\x12\x00\x1d\x00\x39\x00\x35\x00\x12\x00\x05\x00\x30\x00\x33\x00\x05\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x31\x00\x12\x00\x05\x00\x05\x00\x12\x00\x05\x00\x12\x00\x05\x00\x12\x00\x05\x00\x12\x00\x01\x00\x05\x00\x05\x00\xff\xff\x36\x00\x28\x00\x2e\x00\x1d\x00\x1e\x00\x24\x00\x10\x00\x1b\x00\x02\x00\x03\x00\x1f\x00\x29\x00\x03\x00\x1f\x00\x39\x00\x2c\x00\x33\x00\x2c\x00\x35\x00\x2c\x00\x37\x00\x2d\x00\x10\x00\x2d\x00\x10\x00\x2d\x00\x10\x00\x2b\x00\x2c\x00\x21\x00\x18\x00\x31\x00\x19\x00\x16\x00\x2d\x00\x10\x00\x17\x00\x18\x00\x2a\x00\x19\x00\x0f\x00\x10\x00\x12\x00\x10\x00\x13\x00\x10\x00\x05\x00\x06\x00\x25\x00\x20\x00\x22\x00\x23\x00\x14\x00\x00\x00\x15\x00\x1a\x00"#

happyReduceArr = array (1, 32) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32)
	]

happy_n_terms = 18 :: Int
happy_n_nonterms = 14 :: Int

happyReduce_1 = happyReduce 5# 4# happyReduction_1
happyReduction_1 (happy_x_5 :
	happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = happyOut16 happy_x_1; 
	happy_var_2 = happyOut10 happy_x_2; 
	happy_var_4 = happyOut5 happy_x_4; 
	happy_var_5 = happyOut16 happy_x_5; 
	} in
		happyIn4
		 (AbsSyn happy_var_1 (reverse happy_var_2) (reverse happy_var_4) happy_var_5
	) : happyRest
happyReduction_1 _ = notHappyAtAll 

happyReduce_2 = happySpecReduce_2 5# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = happyOut5 happy_x_1; 
	happy_var_2 = happyOut6 happy_x_2; 
	} in
		happyIn5
		 (happy_var_2 : happy_var_1
	)

happyReduce_3 = happySpecReduce_1 5# happyReduction_3
happyReduction_3 happy_x_1
	 =  let {
	happy_var_1 = happyOut6 happy_x_1; 
	} in
		happyIn5
		 ([happy_var_1]
	)

happyReduce_4 = happyReduce 5# 6# happyReduction_4
happyReduction_4 (happy_x_5 :
	happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = happyOut15 happy_x_1; 
	happy_var_3 = happyOut17 happy_x_3; 
	happy_var_5 = happyOut7 happy_x_5; 
	} in
		happyIn6
		 ((happy_var_1,happy_var_5,Just happy_var_3)
	) : happyRest
happyReduction_4 _ = notHappyAtAll 

happyReduce_5 = happyReduce 6# 6# happyReduction_5
happyReduction_5 (happy_x_6 :
	happy_x_5 :
	happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_1 = happyOut15 happy_x_1; 
	happy_var_3 = happyOut17 happy_x_3; 
	happy_var_6 = happyOut7 happy_x_6; 
	} in
		happyIn6
		 ((happy_var_1,happy_var_6,Just happy_var_3)
	) : happyRest
happyReduction_5 _ = notHappyAtAll 

happyReduce_6 = happySpecReduce_3 6# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = happyOut15 happy_x_1; 
	happy_var_3 = happyOut7 happy_x_3; 
	} in
		happyIn6
		 ((happy_var_1,happy_var_3,Nothing)
	)

happyReduce_7 = happySpecReduce_3 7# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = happyOut8 happy_x_1; 
	happy_var_3 = happyOut7 happy_x_3; 
	} in
		happyIn7
		 (happy_var_1 : happy_var_3
	)

happyReduce_8 = happySpecReduce_1 7# happyReduction_8
happyReduction_8 happy_x_1
	 =  let {
	happy_var_1 = happyOut8 happy_x_1; 
	} in
		happyIn7
		 ([happy_var_1]
	)

happyReduce_9 = happyMonadReduce 4# 8# happyReduction_9
happyReduction_9 (happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = happyThen (let {
	happy_var_1 = happyOut14 happy_x_1; 
	happy_var_2 = happyOut9 happy_x_2; 
	happy_var_3 = happyOut17 happy_x_3; 
	} in
		 \s l -> returnP (happy_var_1,happy_var_3,l,happy_var_2) s l
	) (\r -> happyReturn (happyIn8 r))
happyReduction_9 _ = notHappyAtAll 

happyReduce_10 = happyMonadReduce 3# 8# happyReduction_10
happyReduction_10 (happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = happyThen (let {
	happy_var_1 = happyOut14 happy_x_1; 
	happy_var_2 = happyOut9 happy_x_2; 
	happy_var_3 = happyOut17 happy_x_3; 
	} in
		 \s l -> returnP (happy_var_1,happy_var_3,l,happy_var_2) s l
	) (\r -> happyReturn (happyIn8 r))
happyReduction_10 _ = notHappyAtAll 

happyReduce_11 = happySpecReduce_2 9# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut15 happy_x_2; 
	} in
		happyIn9
		 (Just happy_var_2
	)

happyReduce_12 = happySpecReduce_0 9# happyReduction_12
happyReduction_12  =  happyIn9
		 (Nothing
	)

happyReduce_13 = happySpecReduce_2 10# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = happyOut10 happy_x_1; 
	happy_var_2 = happyOut11 happy_x_2; 
	} in
		happyIn10
		 (happy_var_2 : happy_var_1
	)

happyReduce_14 = happySpecReduce_1 10# happyReduction_14
happyReduction_14 happy_x_1
	 =  let {
	happy_var_1 = happyOut11 happy_x_1; 
	} in
		happyIn10
		 ([happy_var_1]
	)

happyReduce_15 = happySpecReduce_2 11# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut17 happy_x_2; 
	} in
		happyIn11
		 (TokenType happy_var_2
	)

happyReduce_16 = happySpecReduce_2 11# happyReduction_16
happyReduction_16 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut12 happy_x_2; 
	} in
		happyIn11
		 (TokenSpec happy_var_2
	)

happyReduce_17 = happySpecReduce_2 11# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut15 happy_x_2; 
	} in
		happyIn11
		 (TokenName happy_var_2
	)

happyReduce_18 = happySpecReduce_3 11# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut17 happy_x_2; 
	happy_var_3 = happyOut17 happy_x_3; 
	} in
		happyIn11
		 (TokenLexer happy_var_2 happy_var_3
	)

happyReduce_19 = happySpecReduce_2 11# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut17 happy_x_2; 
	} in
		happyIn11
		 (TokenMonad happy_var_2 ">>=" "return"
	)

happyReduce_20 = happyReduce 4# 11# happyReduction_20
happyReduction_20 (happy_x_4 :
	happy_x_3 :
	happy_x_2 :
	happy_x_1 :
	happyRest)
	 = let {
	happy_var_2 = happyOut17 happy_x_2; 
	happy_var_3 = happyOut17 happy_x_3; 
	happy_var_4 = happyOut17 happy_x_4; 
	} in
		happyIn11
		 (TokenMonad happy_var_2 happy_var_3 happy_var_4
	) : happyRest
happyReduction_20 _ = notHappyAtAll 

happyReduce_21 = happySpecReduce_2 11# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut14 happy_x_2; 
	} in
		happyIn11
		 (TokenNonassoc happy_var_2
	)

happyReduce_22 = happySpecReduce_2 11# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut14 happy_x_2; 
	} in
		happyIn11
		 (TokenRight happy_var_2
	)

happyReduce_23 = happySpecReduce_2 11# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  let {
	happy_var_2 = happyOut14 happy_x_2; 
	} in
		happyIn11
		 (TokenLeft happy_var_2
	)

happyReduce_24 = happySpecReduce_2 12# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = happyOut13 happy_x_1; 
	happy_var_2 = happyOut12 happy_x_2; 
	} in
		happyIn12
		 (happy_var_1:happy_var_2
	)

happyReduce_25 = happySpecReduce_1 12# happyReduction_25
happyReduction_25 happy_x_1
	 =  let {
	happy_var_1 = happyOut13 happy_x_1; 
	} in
		happyIn12
		 ([happy_var_1]
	)

happyReduce_26 = happySpecReduce_2 13# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = happyOut15 happy_x_1; 
	happy_var_2 = happyOut17 happy_x_2; 
	} in
		happyIn13
		 ((happy_var_1,happy_var_2)
	)

happyReduce_27 = happySpecReduce_2 14# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  let {
	happy_var_1 = happyOut15 happy_x_1; 
	happy_var_2 = happyOut14 happy_x_2; 
	} in
		happyIn14
		 (happy_var_1 : happy_var_2
	)

happyReduce_28 = happySpecReduce_0 14# happyReduction_28
happyReduction_28  =  happyIn14
		 ([]
	)

happyReduce_29 = happySpecReduce_1 15# happyReduction_29
happyReduction_29 happy_x_1
	 =  let {
	(TokenInfo happy_var_1 TokId) = happyOutTok happy_x_1; 
	} in
		happyIn15
		 (happy_var_1
	)

happyReduce_30 = happySpecReduce_1 16# happyReduction_30
happyReduction_30 happy_x_1
	 =  let {
	happy_var_1 = happyOut17 happy_x_1; 
	} in
		happyIn16
		 (Just happy_var_1
	)

happyReduce_31 = happySpecReduce_0 16# happyReduction_31
happyReduction_31  =  happyIn16
		 (Nothing
	)

happyReduce_32 = happySpecReduce_1 17# happyReduction_32
happyReduction_32 happy_x_1
	 =  let {
	(TokenInfo happy_var_1 TokCodeQuote) = happyOutTok happy_x_1; 
	} in
		happyIn17
		 (happy_var_1
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TokenEOF -> happyDoAction 17# (error "reading EOF!") action sts stk;
	TokenInfo happy_dollar_dollar TokId -> cont 1#;
	TokenKW     TokSpecId_TokenType -> cont 2#;
	TokenKW     TokSpecId_Token -> cont 3#;
	TokenKW     TokSpecId_Name -> cont 4#;
	TokenKW     TokSpecId_Lexer -> cont 5#;
	TokenKW     TokSpecId_Monad -> cont 6#;
	TokenKW     TokSpecId_Nonassoc -> cont 7#;
	TokenKW     TokSpecId_Left -> cont 8#;
	TokenKW     TokSpecId_Right -> cont 9#;
	TokenKW     TokSpecId_Prec -> cont 10#;
	TokenInfo happy_dollar_dollar TokCodeQuote -> cont 11#;
	TokenKW     TokColon -> cont 12#;
	TokenKW     TokSemiColon -> cont 13#;
	TokenKW     TokDoubleColon -> cont 14#;
	TokenKW     TokDoublePercent -> cont 15#;
	TokenKW     TokBar -> cont 16#;
	})

happyThen :: P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn = (returnP)
ourParser = happyParse

happyError :: P a
happyError s l = failP (show l ++ ": Parse error\n") s l{-# LINE 1 "GenericTemplate.hs" -}
{-# LINE 1 "GenericTemplate.hs" -}
-- $Id: Parser.hs,v 1.13 2000/07/12 16:21:44 simonmar Exp $













{-# LINE 27 "GenericTemplate.hs" -}



data Happy_IntList = HappyNil | HappyCons Int# Happy_IntList












































-----------------------------------------------------------------------------
-- starting the parse

happyParse = happyNewToken 0# HappyNil []

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# 0#) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# 1#)))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# 1#)
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# 0#) 
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (A# arr) off =
	(i `iShiftL#` 16#) `iShiftRA#` 16#
  where
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 138 "GenericTemplate.hs" -}


-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts [ ans ] = happyReturn (happyOut4 (ans))
happyAccept j tk st sts _       = (happyTcHack j 
				         (happyTcHack st))
				  notHappyAtAll

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x : _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk)):stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn : stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1:stk')
     = happyGoto nt j tk st sts (fn v1 : stk')
happySpecReduce_1 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1:v2:stk')
     = happyGoto nt j tk st sts (fn v1 v2 : stk')
happySpecReduce_2 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1:v2:v3:stk')
     = happyGoto nt j tk st sts (fn v1 v2 v3 : stk')
happySpecReduce_3 _ _ _ _ _ _ _
     = notHappyAtAll

happyRedcue k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk = happyGoto nt j tk st1 sts1 (fn stk)
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r : drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = drop (I# (k)) stk

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# 1#) t

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


-- subtract 4 from nt, because nonterminals start at 4 (see Grammar.hs)
happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where i      = (nt -# 4#)
	 off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# i)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st HappyNil stk =
--	trace "failing" $ 
    	happyError

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok : _ : stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok:stk))

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) : stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
