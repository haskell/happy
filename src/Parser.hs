{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -w #-}
module Parser (ourParser,AbsSyn) where
import ParseMonad
import AbsSyn
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (AbsSyn) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (AbsSyn)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([Rule]) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([Rule])
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Rule) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Rule)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ([String]) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ([String])
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ([String]) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ([String])
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([Prod]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([Prod])
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Prod) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Prod)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Term) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Term)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([Term]) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([Term])
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([Term]) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([Term])
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Term]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Term])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Maybe String) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Maybe String)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([Directive String]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([Directive String])
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Directive String) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Directive String)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Maybe String) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Maybe String)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([(String,String)]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([(String,String)])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ((String,String)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ((String,String))
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([String]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([String])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Maybe String) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Maybe String)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x73\x00\x73\x00\x23\x00\x00\x00\x71\x00\xff\xff\x00\x00\x70\x00\x7a\x00\x79\x00\x76\x00\x6f\x00\x00\x00\x6b\x00\x75\x00\x75\x00\x75\x00\x69\x00\x67\x00\x72\x00\x6e\x00\x66\x00\x00\x00\x63\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x62\x00\x60\x00\x6c\x00\x6c\x00\x00\x00\x6a\x00\x5f\x00\x00\x00\x00\x00\x68\x00\x12\x00\x00\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x5a\x00\x09\x00\x65\x00\x00\x00\x00\x00\x33\x00\x00\x00\x64\x00\x52\x00\x00\x00\x0a\x00\x00\x00\x50\x00\x00\x00\x57\x00\x61\x00\x4e\x00\x00\x00\x5d\x00\x00\x00\x5c\x00\x00\x00\x4f\x00\x5b\x00\x59\x00\x4c\x00\x58\x00\x00\x00\x58\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x31\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x11\x00\x49\x00\x3d\x00\x00\x00\x00\x00\x48\x00\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x46\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x43\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x10\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x35\x00\x00\x00\x30\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xca\xff\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\x00\x00\xcc\xff\xcc\xff\xcc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\xd5\xff\xd6\xff\xd7\xff\xd9\xff\xcc\xff\xd8\xff\xda\xff\xde\xff\x00\x00\xd1\xff\xd1\xff\xe3\xff\xcf\xff\x00\x00\xe4\xff\xe6\xff\x00\x00\xca\xff\xfc\xff\xf7\xff\xce\xff\xd0\xff\xe2\xff\xd2\xff\xe1\xff\xdf\xff\xdd\xff\xcd\xff\xd3\xff\xdc\xff\x00\x00\x00\x00\xfd\xff\xfe\xff\x00\x00\xf6\xff\xed\xff\x00\x00\xdb\xff\x00\x00\xf9\xff\xf3\xff\xec\xff\xe7\xff\xee\xff\xf0\xff\xf8\xff\x00\x00\xf5\xff\x00\x00\xeb\xff\x00\x00\x00\x00\xed\xff\x00\x00\xed\xff\xfb\xff\xed\xff\xf4\xff\xe8\xff\xf1\xff\xea\xff\x00\x00\xef\xff\x00\x00\xf2\xff\xfa\xff\xe9\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x01\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x02\x00\x01\x00\x07\x00\x07\x00\x17\x00\x0a\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x14\x00\x14\x00\x16\x00\x01\x00\x02\x00\x12\x00\x12\x00\x12\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x07\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0b\x00\x0f\x00\x10\x00\x0f\x00\x10\x00\x0c\x00\x0d\x00\x1a\x00\x1b\x00\x1a\x00\x1b\x00\x04\x00\x03\x00\x0e\x00\x0e\x00\x11\x00\x01\x00\x0d\x00\x11\x00\x11\x00\x11\x00\x01\x00\x01\x00\x12\x00\x01\x00\x01\x00\x01\x00\x15\x00\x14\x00\x12\x00\x01\x00\x0c\x00\x12\x00\x01\x00\x01\x00\x19\x00\x18\x00\x01\x00\x19\x00\x01\x00\x12\x00\x01\x00\x01\x00\x01\x00\x12\x00\x12\x00\x12\x00\x01\x00\x12\x00\x12\x00\x01\x00\x01\x00\x12\x00\x12\x00\x01\x00\x01\x00\x13\x00\x12\x00\xff\xff\xff\xff\xff\xff\x12\x00\x12\x00\xff\xff\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x4f\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x04\x00\x38\x00\x2c\x00\x55\x00\x5b\x00\x29\x00\x56\x00\x5a\x00\x41\x00\x42\x00\x43\x00\x44\x00\x3d\x00\x50\x00\x3e\x00\x29\x00\x2a\x00\x39\x00\x02\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x4a\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x50\x00\x41\x00\x42\x00\x43\x00\x44\x00\x52\x00\x41\x00\x42\x00\x43\x00\x44\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x4b\x00\x2d\x00\x24\x00\x23\x00\x24\x00\x05\x00\x06\x00\x58\x00\x59\x00\x47\x00\x48\x00\x3a\x00\x36\x00\x2e\x00\x30\x00\x33\x00\x46\x00\x27\x00\x1b\x00\x1d\x00\x1e\x00\x46\x00\x46\x00\x02\x00\x54\x00\x46\x00\x49\x00\x5a\x00\x52\x00\x55\x00\x46\x00\x4d\x00\x40\x00\x46\x00\x3c\x00\x4a\x00\x4e\x00\x2c\x00\x38\x00\x26\x00\x3f\x00\x30\x00\x1d\x00\x18\x00\x36\x00\x2d\x00\x32\x00\x19\x00\x33\x00\x35\x00\x1d\x00\x22\x00\x17\x00\x1a\x00\x23\x00\x26\x00\x1b\x00\x20\x00\x00\x00\x00\x00\x00\x00\x21\x00\x27\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 53) [
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
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53)
	]

happy_n_terms = 29 :: Int
happy_n_nonterms = 19 :: Int

happyReduce_1 = happyReduce 5# 0# happyReduction_1
happyReduction_1 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOut22 happy_x_5 of { happy_var_5 -> 
	happyIn4
		 (AbsSyn happy_var_1 (reverse happy_var_2) (reverse happy_var_4) happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_2 = happySpecReduce_2  1# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([happy_var_1]
	)}

happyReduce_4 = happyReduce 6# 2# happyReduction_4
happyReduction_4 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
	case happyOut9 happy_x_6 of { happy_var_6 -> 
	happyIn6
		 ((happy_var_1,happy_var_2,happy_var_6,Just happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_5 = happyReduce 7# 2# happyReduction_5
happyReduction_5 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
	case happyOut9 happy_x_7 of { happy_var_7 -> 
	happyIn6
		 ((happy_var_1,happy_var_2,happy_var_7,Just happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_6 = happyReduce 4# 2# happyReduction_6
happyReduction_6 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn6
		 ((happy_var_1,happy_var_2,happy_var_4,Nothing)
	) `HappyStk` happyRest}}}

happyReduce_7 = happySpecReduce_3  3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (reverse happy_var_2
	)}

happyReduce_8 = happySpecReduce_0  3# happyReduction_8
happyReduction_8  =  happyIn7
		 ([]
	)

happyReduce_9 = happySpecReduce_1  4# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	happyIn8
		 ([happy_var_1]
	)}

happyReduce_10 = happySpecReduce_3  4# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokId) -> 
	happyIn8
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_11 = happySpecReduce_3  5# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_12 = happySpecReduce_1  5# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ([happy_var_1]
	)}

happyReduce_13 = happyMonadReduce 4# 6# happyReduction_13
happyReduction_13 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	( lineP >>= \l -> return (happy_var_1,happy_var_3,l,happy_var_2))}}}
	) (\r -> happyReturn (happyIn10 r))

happyReduce_14 = happyMonadReduce 3# 6# happyReduction_14
happyReduction_14 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	( lineP >>= \l -> return (happy_var_1,happy_var_3,l,happy_var_2))}}}
	) (\r -> happyReturn (happyIn10 r))

happyReduce_15 = happySpecReduce_1  7# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	happyIn11
		 (App happy_var_1 []
	)}

happyReduce_16 = happyReduce 4# 7# happyReduction_16
happyReduction_16 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (App happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_17 = happySpecReduce_1  8# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (reverse happy_var_1
	)}

happyReduce_18 = happySpecReduce_0  8# happyReduction_18
happyReduction_18  =  happyIn12
		 ([]
	)

happyReduce_19 = happySpecReduce_1  9# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ([happy_var_1]
	)}

happyReduce_20 = happySpecReduce_2  9# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_21 = happySpecReduce_1  10# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_22 = happySpecReduce_3  10# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_23 = happySpecReduce_2  11# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	happyIn15
		 (Just happy_var_2
	)}

happyReduce_24 = happySpecReduce_0  11# happyReduction_24
happyReduction_24  =  happyIn15
		 (Nothing
	)

happyReduce_25 = happySpecReduce_2  12# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_26 = happySpecReduce_1  12# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ([happy_var_1]
	)}

happyReduce_27 = happySpecReduce_2  13# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn17
		 (TokenType happy_var_2
	)}

happyReduce_28 = happySpecReduce_2  13# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (TokenSpec happy_var_2
	)}

happyReduce_29 = happySpecReduce_3  13# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (TokenName happy_var_2 happy_var_3 False
	)}}

happyReduce_30 = happySpecReduce_3  13# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (TokenName happy_var_2 happy_var_3 True
	)}}

happyReduce_31 = happySpecReduce_1  13# happyReduction_31
happyReduction_31 happy_x_1
	 =  happyIn17
		 (TokenImportedIdentity
	)

happyReduce_32 = happySpecReduce_3  13# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	happyIn17
		 (TokenLexer happy_var_2 happy_var_3
	)}}

happyReduce_33 = happySpecReduce_2  13# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn17
		 (TokenMonad "()" happy_var_2 ">>=" "return"
	)}

happyReduce_34 = happySpecReduce_3  13# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	happyIn17
		 (TokenMonad happy_var_2 happy_var_3 ">>=" "return"
	)}}

happyReduce_35 = happyReduce 4# 13# happyReduction_35
happyReduction_35 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
	happyIn17
		 (TokenMonad "()" happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_36 = happyReduce 5# 13# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
	case happyOutTok happy_x_5 of { (TokenInfo happy_var_5 TokCodeQuote) -> 
	happyIn17
		 (TokenMonad happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_37 = happySpecReduce_2  13# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (TokenNonassoc happy_var_2
	)}

happyReduce_38 = happySpecReduce_2  13# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (TokenRight happy_var_2
	)}

happyReduce_39 = happySpecReduce_2  13# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (TokenLeft happy_var_2
	)}

happyReduce_40 = happySpecReduce_2  13# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenNum happy_var_2  TokNum) -> 
	happyIn17
		 (TokenExpect happy_var_2
	)}

happyReduce_41 = happySpecReduce_2  13# happyReduction_41
happyReduction_41 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn17
		 (TokenError happy_var_2
	)}

happyReduce_42 = happySpecReduce_2  13# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	happyIn17
		 (TokenErrorHandlerType happy_var_2
	)}

happyReduce_43 = happySpecReduce_2  13# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn17
		 (TokenAttributetype happy_var_2
	)}

happyReduce_44 = happySpecReduce_3  13# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
	case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
	happyIn17
		 (TokenAttribute happy_var_2 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_1  14# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	happyIn18
		 (Just happy_var_1
	)}

happyReduce_46 = happySpecReduce_0  14# happyReduction_46
happyReduction_46  =  happyIn18
		 (Nothing
	)

happyReduce_47 = happySpecReduce_2  15# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (happy_var_1:happy_var_2
	)}}

happyReduce_48 = happySpecReduce_1  15# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ([happy_var_1]
	)}

happyReduce_49 = happySpecReduce_2  16# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
	happyIn20
		 ((happy_var_1,happy_var_2)
	)}}

happyReduce_50 = happySpecReduce_2  17# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
	case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_51 = happySpecReduce_0  17# happyReduction_51
happyReduction_51  =  happyIn21
		 ([]
	)

happyReduce_52 = happySpecReduce_1  18# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokCodeQuote) -> 
	happyIn22
		 (Just happy_var_1
	)}

happyReduce_53 = happySpecReduce_0  18# happyReduction_53
happyReduction_53  =  happyIn22
		 (Nothing
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TokenEOF -> happyDoAction 28# tk action sts stk;
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
	TokenKW      TokSpecId_ErrorHandlerType -> cont 15#;
	TokenKW      TokSpecId_Attribute -> cont 16#;
	TokenKW      TokSpecId_Attributetype -> cont 17#;
	TokenInfo happy_dollar_dollar TokCodeQuote -> cont 18#;
	TokenNum happy_dollar_dollar  TokNum -> cont 19#;
	TokenKW      TokColon -> cont 20#;
	TokenKW      TokSemiColon -> cont 21#;
	TokenKW      TokDoubleColon -> cont 22#;
	TokenKW      TokDoublePercent -> cont 23#;
	TokenKW      TokBar -> cont 24#;
	TokenKW      TokParenL -> cont 25#;
	TokenKW      TokParenR -> cont 26#;
	TokenKW      TokComma -> cont 27#;
	_ -> happyError' tk
	})

happyError_ 28# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = (\token -> happyError) tk

ourParser = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


happyError :: P a
happyError = lineP >>= \l -> fail (show l ++ ": Parse error\n")
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
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
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

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
