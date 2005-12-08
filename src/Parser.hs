{-# OPTIONS -fglasgow-exts -cpp #-}
module Parser (ourParser,AbsSyn) where
import ParseMonad
import AbsSyn
import Lexer
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.16

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
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
happyIn12 :: (Maybe String) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Maybe String)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([(String,String)]) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([(String,String)])
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ((String,String)) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ((String,String))
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([String]) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([String])
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Maybe String) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Maybe String)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x4e\x00\x4e\x00\x1b\x00\x00\x00\x4b\x00\xff\xff\x00\x00\x4d\x00\x5a\x00\x59\x00\x56\x00\x4a\x00\x00\x00\x49\x00\x54\x00\x54\x00\x54\x00\x46\x00\x45\x00\x00\x00\x00\x00\x00\x00\x51\x00\x00\x00\x00\x00\x44\x00\x42\x00\x4f\x00\x4f\x00\x00\x00\x4c\x00\x3f\x00\x00\x00\x00\x00\x48\x00\x0d\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x3c\x00\x47\x00\x3b\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x3a\x00\x39\x00\x00\x00\x38\x00\x43\x00\x41\x00\x35\x00\x3e\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x27\x00\x37\x00\x10\x00\x00\x00\x00\x00\x34\x00\x00\x00\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x32\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x2a\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x13\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x2b\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd9\xff\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\xff\x00\x00\xdb\xff\xdb\xff\xdb\xff\x00\x00\x00\x00\xe2\xff\xe3\xff\xe5\xff\xdb\xff\xe4\xff\xe6\xff\xea\xff\x00\x00\xe0\xff\xe0\xff\xef\xff\xde\xff\x00\x00\xf0\xff\xf2\xff\x00\x00\xd9\xff\xfc\xff\x00\x00\xdd\xff\xdf\xff\xee\xff\xe1\xff\xed\xff\xeb\xff\xe9\xff\xdc\xff\xe8\xff\xdb\xff\x00\x00\xfd\xff\xfe\xff\x00\x00\xf9\xff\xf7\xff\xf3\xff\xe7\xff\x00\x00\x00\x00\xdb\xff\x00\x00\xdb\xff\xfb\xff\xdb\xff\xf8\xff\xf4\xff\xf5\xff\xf6\xff\xfa\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x01\x00\x0d\x00\x0e\x00\x01\x00\x11\x00\x02\x00\x13\x00\x0b\x00\x14\x00\x01\x00\x02\x00\x06\x00\x07\x00\x09\x00\x0a\x00\x0c\x00\x11\x00\x0f\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x0d\x00\x0e\x00\x09\x00\x0a\x00\x03\x00\x04\x00\x03\x00\x04\x00\x03\x00\x04\x00\x08\x00\x0c\x00\x0b\x00\x05\x00\x0b\x00\x08\x00\x0b\x00\x03\x00\x04\x00\x07\x00\x0b\x00\x0b\x00\x0b\x00\x01\x00\x12\x00\x0b\x00\x01\x00\x0c\x00\x01\x00\x0c\x00\x11\x00\x0f\x00\x01\x00\x01\x00\x0f\x00\x0f\x00\x0f\x00\x01\x00\x0f\x00\x15\x00\x01\x00\x0f\x00\x01\x00\x0f\x00\x0f\x00\x01\x00\x10\x00\x01\x00\x0f\x00\x0f\x00\x01\x00\x01\x00\x0f\x00\x0f\x00\xff\xff\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x3c\x00\x12\x00\x13\x00\x26\x00\x30\x00\x31\x00\x31\x00\x2d\x00\x23\x00\x23\x00\x24\x00\x05\x00\x06\x00\x27\x00\x1e\x00\x32\x00\x3d\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x04\x00\x12\x00\x13\x00\x1d\x00\x1e\x00\x43\x00\x35\x00\x3d\x00\x35\x00\x3f\x00\x35\x00\x28\x00\x02\x00\x36\x00\x38\x00\x36\x00\x2a\x00\x36\x00\x34\x00\x35\x00\x21\x00\x15\x00\x17\x00\x18\x00\x17\x00\x43\x00\x36\x00\x17\x00\x02\x00\x41\x00\x3a\x00\x3f\x00\x42\x00\x17\x00\x26\x00\x34\x00\x38\x00\x2f\x00\x20\x00\x27\x00\x3b\x00\x2a\x00\x2c\x00\x17\x00\x2d\x00\x14\x00\x17\x00\x15\x00\x1c\x00\x1a\x00\x1b\x00\x1d\x00\x20\x00\x21\x00\x04\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 38) [
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
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38)
	]

happy_n_terms = 23 :: Int
happy_n_nonterms = 13 :: Int

happyReduce_1 = happyReduce 5# 0# happyReduction_1
happyReduction_1 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	happyIn4
		 (AbsSyn happy_var_1 (reverse happy_var_2) (reverse happy_var_4) happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_2 = happySpecReduce_2 1# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_3 = happySpecReduce_1 1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([happy_var_1]
	)}

happyReduce_4 = happyReduce 5# 2# happyReduction_4
happyReduction_4 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	case happyOut7 happy_x_5 of { happy_var_5 -> 
	happyIn6
		 ((happy_var_1,happy_var_5,Just happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_5 = happyReduce 6# 2# happyReduction_5
happyReduction_5 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	case happyOut7 happy_x_6 of { happy_var_6 -> 
	happyIn6
		 ((happy_var_1,happy_var_6,Just happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_6 = happySpecReduce_3 2# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 ((happy_var_1,happy_var_3,Nothing)
	)}}

happyReduce_7 = happySpecReduce_3 3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_8 = happySpecReduce_1 3# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ([happy_var_1]
	)}

happyReduce_9 = happyMonadReduce 4# 4# happyReduction_9
happyReduction_9 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	 lineP >>= \l -> return (happy_var_1,happy_var_3,l,happy_var_2)}}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_10 = happyMonadReduce 3# 4# happyReduction_10
happyReduction_10 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	 lineP >>= \l -> return (happy_var_1,happy_var_3,l,happy_var_2)}}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_11 = happySpecReduce_2 5# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	happyIn9
		 (Just happy_var_2
	)}

happyReduce_12 = happySpecReduce_0 5# happyReduction_12
happyReduction_12  =  happyIn9
		 (Nothing
	)

happyReduce_13 = happySpecReduce_2 6# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_14 = happySpecReduce_1 6# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ([happy_var_1]
	)}

happyReduce_15 = happySpecReduce_2 7# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn11
		 (TokenType happy_var_2
	)}

happyReduce_16 = happySpecReduce_2 7# happyReduction_16
happyReduction_16 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (TokenSpec happy_var_2
	)}

happyReduce_17 = happySpecReduce_3 7# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (TokenName happy_var_2 happy_var_3 False
	)}}

happyReduce_18 = happySpecReduce_3 7# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (TokenName happy_var_2 happy_var_3 True
	)}}

happyReduce_19 = happySpecReduce_1 7# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn11
		 (TokenImportedIdentity
	)

happyReduce_20 = happySpecReduce_3 7# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	happyIn11
		 (TokenLexer happy_var_2 happy_var_3
	)}}

happyReduce_21 = happySpecReduce_2 7# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn11
		 (TokenMonad "()" happy_var_2 ">>=" "return"
	)}

happyReduce_22 = happySpecReduce_3 7# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	happyIn11
		 (TokenMonad happy_var_2 happy_var_3 ">>=" "return"
	)}}

happyReduce_23 = happyReduce 4# 7# happyReduction_23
happyReduction_23 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
	happyIn11
		 (TokenMonad "()" happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_24 = happyReduce 5# 7# happyReduction_24
happyReduction_24 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
	case happyOutTok happy_x_5 of { (TokenInfo happy_var_5 TokCodeQuote) -> 
	happyIn11
		 (TokenMonad happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_25 = happySpecReduce_2 7# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (TokenNonassoc happy_var_2
	)}

happyReduce_26 = happySpecReduce_2 7# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (TokenRight happy_var_2
	)}

happyReduce_27 = happySpecReduce_2 7# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (TokenLeft happy_var_2
	)}

happyReduce_28 = happySpecReduce_2 7# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenNum happy_var_2  TokNum) -> 
	happyIn11
		 (TokenExpect happy_var_2
	)}

happyReduce_29 = happySpecReduce_2 7# happyReduction_29
happyReduction_29 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn11
		 (TokenError happy_var_2
	)}

happyReduce_30 = happySpecReduce_1 8# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	happyIn12
		 (Just happy_var_1
	)}

happyReduce_31 = happySpecReduce_0 8# happyReduction_31
happyReduction_31  =  happyIn12
		 (Nothing
	)

happyReduce_32 = happySpecReduce_2 9# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (happy_var_1:happy_var_2
	)}}

happyReduce_33 = happySpecReduce_1 9# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ([happy_var_1]
	)}

happyReduce_34 = happySpecReduce_2 10# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn14
		 ((happy_var_1,happy_var_2)
	)}}

happyReduce_35 = happySpecReduce_2 11# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_36 = happySpecReduce_0 11# happyReduction_36
happyReduction_36  =  happyIn15
		 ([]
	)

happyReduce_37 = happySpecReduce_1 12# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokCodeQuote) -> 
	happyIn16
		 (Just happy_var_1
	)}

happyReduce_38 = happySpecReduce_0 12# happyReduction_38
happyReduction_38  =  happyIn16
		 (Nothing
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TokenEOF -> happyDoAction 22# (error "reading EOF!") action sts stk;
	TokenInfo happy_dollar_dollar TokId -> cont 1#;
	TokenKW      TokSpecId_TokenType -> cont 2#;
	TokenKW      TokSpecId_Token -> cont 3#;
	TokenKW      TokSpecId_Name -> cont 4#;
	TokenKW      TokSpecId_Partial -> cont 5#;
	TokenKW      TokSpecId_Lexer -> cont 6#;
	TokenKW      TokSpecId_ImportedIdentity -> cont 7#;
	TokenKW      TokSpecId_Monad -> cont 8#;
	TokenKW      TokSpecId_Nonassoc -> cont 9#;
	TokenKW      TokSpecId_Left -> cont 10#;
	TokenKW      TokSpecId_Right -> cont 11#;
	TokenKW      TokSpecId_Prec -> cont 12#;
	TokenKW      TokSpecId_Expect -> cont 13#;
	TokenKW      TokSpecId_Error -> cont 14#;
	TokenInfo happy_dollar_dollar TokCodeQuote -> cont 15#;
	TokenNum happy_dollar_dollar  TokNum -> cont 16#;
	TokenKW      TokColon -> cont 17#;
	TokenKW      TokSemiColon -> cont 18#;
	TokenKW      TokDoubleColon -> cont 19#;
	TokenKW      TokDoublePercent -> cont 20#;
	TokenKW      TokBar -> cont 21#;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => Token -> P a
happyError' tk = (\token -> happyError) tk

ourParser = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq

happyError :: P a
happyError = lineP >>= \l -> fail (show l ++ ": Parse error\n")
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "GenericTemplate.hs" #-}

{-# LINE 59 "GenericTemplate.hs" #-}

{-# LINE 68 "GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

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
