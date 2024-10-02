{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -w #-}
module Happy.Frontend.Parser (ourParser) where
import Happy.Frontend.ParseMonad.Class
import Happy.Frontend.ParseMonad
import Happy.Frontend.AbsSyn
import Happy.Frontend.Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.List as Happy_Data_List
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap5 = HappyWrap5 (BookendedAbsSyn)
happyIn5 :: (BookendedAbsSyn) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (AbsSyn String)
happyIn6 :: (AbsSyn String) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ([Rule String])
happyIn7 :: ([Rule String]) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (Rule String)
happyIn8 :: (Rule String) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ([String])
happyIn9 :: ([String]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ([String])
happyIn10 :: ([String]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 ([Prod String])
happyIn11 :: ([Prod String]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Prod String)
happyIn12 :: (Prod String) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (Term)
happyIn13 :: (Term) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ([Term])
happyIn14 :: ([Term]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ([Term])
happyIn15 :: ([Term]) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ([Term])
happyIn16 :: ([Term]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (Prec)
happyIn17 :: (Prec) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ([Directive String])
happyIn18 :: ([Directive String]) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (Directive String)
happyIn19 :: (Directive String) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (Maybe String)
happyIn20 :: (Maybe String) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ([(String, TokenSpec)])
happyIn21 :: ([(String, TokenSpec)]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 ((String, TokenSpec))
happyIn22 :: ((String, TokenSpec)) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ([String])
happyIn23 :: ([String]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (Maybe String)
happyIn24 :: (Maybe String) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["id","spec_tokentype","spec_token","spec_name","spec_partial","spec_lexer","spec_imported_identity","spec_monad","spec_nonassoc","spec_left","spec_right","spec_prec","spec_shift","spec_expect","spec_error","spec_errorexpected","spec_attribute","spec_attributetype","code","int","\":\"","\";\"","\"::\"","\"%%\"","\"|\"","\"(\"","\")\"","\",\"","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x02\x00\x00\x00\x02\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xff\xff\x35\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\x3b\x00\x00\x00\x4c\x00\x00\x00\x4e\x00\x00\x00\x4f\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x52\x00\x00\x00\x52\x00\x00\x00\x52\x00\x00\x00\x40\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x55\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x49\x00\x00\x00\x5c\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x00\x00\x00\xfd\xff\xff\xff\x61\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x64\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\x38\x00\x00\x00\x66\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x57\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x00\x00\x5a\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x12\x00\x00\x00\x62\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x63\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x6e\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x00\x00\x00\x00\x77\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x00\x00\x00\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xc8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc9\xff\xff\xff\x00\x00\x00\x00\xc8\xff\xff\xff\x00\x00\x00\x00\xe3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\xff\xff\x00\x00\x00\x00\xca\xff\xff\xff\xca\xff\xff\xff\xca\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\xff\xff\x00\x00\x00\x00\xc8\xff\xff\xff\xd5\xff\xff\xff\xd7\xff\xff\xff\xca\xff\xff\xff\xd6\xff\xff\xff\xd8\xff\xff\xff\xdc\xff\xff\xff\x00\x00\x00\x00\xcf\xff\xff\xff\xcf\xff\xff\xff\xe1\xff\xff\xff\xcd\xff\xff\xff\x00\x00\x00\x00\xe2\xff\xff\xff\xe4\xff\xff\xff\x00\x00\x00\x00\xfe\xff\xff\xff\xfd\xff\xff\xff\xfb\xff\xff\xff\xf6\xff\xff\xff\xcc\xff\xff\xff\xce\xff\xff\xff\xe0\xff\xff\xff\xd0\xff\xff\xff\xdf\xff\xff\xff\xdd\xff\xff\xff\xdb\xff\xff\xff\xcb\xff\xff\xff\xd4\xff\xff\xff\xd1\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\x00\x00\x00\x00\xf5\xff\xff\xff\xec\xff\xff\xff\x00\x00\x00\x00\xd9\xff\xff\xff\x00\x00\x00\x00\xf8\xff\xff\xff\xf2\xff\xff\xff\xeb\xff\xff\xff\xe5\xff\xff\xff\xed\xff\xff\xff\xef\xff\xff\xff\xf7\xff\xff\xff\x00\x00\x00\x00\xf4\xff\xff\xff\x00\x00\x00\x00\xea\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\xff\xff\xec\xff\xff\xff\x00\x00\x00\x00\xec\xff\xff\xff\xfa\xff\xff\xff\xec\xff\xff\xff\xf3\xff\xff\xff\xe7\xff\xff\xff\xf0\xff\xff\xff\xe9\xff\xff\xff\x00\x00\x00\x00\xee\xff\xff\xff\x00\x00\x00\x00\xf1\xff\xff\xff\xf9\xff\xff\xff\xe8\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x1e\x00\x00\x00\x18\x00\x00\x00\x14\x00\x00\x00\x19\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x16\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x13\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x08\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x0b\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x14\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x1b\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x1a\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x1b\x00\x00\x00\x17\x00\x00\x00\x02\x00\x00\x00\xff\xff\xff\xff\x13\x00\x00\x00\x13\x00\x00\x00\x0e\x00\x00\x00\x13\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\xff\xff\xff\xff\x05\x00\x00\x00\x12\x00\x00\x00\x0c\x00\x00\x00\x12\x00\x00\x00\x12\x00\x00\x00\x12\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x08\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x51\x00\x00\x00\x05\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x04\x00\x00\x00\x3e\x00\x00\x00\xff\xff\xff\xff\x3f\x00\x00\x00\x04\x00\x00\x00\x29\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x52\x00\x00\x00\x54\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x02\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x52\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x5c\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x57\x00\x00\x00\x48\x00\x00\x00\x49\x00\x00\x00\x58\x00\x00\x00\x4e\x00\x00\x00\x4f\x00\x00\x00\x5a\x00\x00\x00\x5b\x00\x00\x00\x04\x00\x00\x00\x2e\x00\x00\x00\x24\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x21\x00\x00\x00\x20\x00\x00\x00\x1d\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x19\x00\x00\x00\x18\x00\x00\x00\x37\x00\x00\x00\x04\x00\x00\x00\x1d\x00\x00\x00\x34\x00\x00\x00\x33\x00\x00\x00\x31\x00\x00\x00\x26\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x3a\x00\x00\x00\x3d\x00\x00\x00\x38\x00\x00\x00\x40\x00\x00\x00\x47\x00\x00\x00\x41\x00\x00\x00\x47\x00\x00\x00\x4a\x00\x00\x00\x47\x00\x00\x00\x57\x00\x00\x00\x56\x00\x00\x00\x47\x00\x00\x00\x50\x00\x00\x00\x47\x00\x00\x00\x54\x00\x00\x00\x4b\x00\x00\x00\x5c\x00\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x29\x00\x00\x00\x27\x00\x00\x00\x35\x00\x00\x00\x3a\x00\x00\x00\x4b\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x1e\x00\x00\x00\x4c\x00\x00\x00\x1d\x00\x00\x00\x1b\x00\x00\x00\x34\x00\x00\x00\x31\x00\x00\x00\x2f\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 55) [
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
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x04\x00\x00\x00\x0e\x00\x00\x00\x05\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00"#

happyCatchStates :: [Int]
happyCatchStates = []

happy_n_terms = 31 :: Prelude.Int
happy_n_nonterms = 20 :: Prelude.Int

happy_n_starts = 1 :: Prelude.Int

happyReduce_1 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
        case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
        case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
        happyIn5
                 (BookendedAbsSyn happy_var_1 happy_var_2 happy_var_3
        )}}}

happyReduce_2 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
        case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
        happyIn6
                 (AbsSyn (reverse happy_var_1) (reverse happy_var_3)
        )}}

happyReduce_3 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_3 = happySpecReduce_2  2# happyReduction_3
happyReduction_3 happy_x_2
        happy_x_1
         =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
        case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
        happyIn7
                 (happy_var_2 : happy_var_1
        )}}

happyReduce_4 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_4 = happySpecReduce_1  2# happyReduction_4
happyReduction_4 happy_x_1
         =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
        happyIn7
                 ([happy_var_1]
        )}

happyReduce_5 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_5 = happyReduce 6# 3# happyReduction_5
happyReduction_5 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
        case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
        case happyOut11 happy_x_6 of { (HappyWrap11 happy_var_6) -> 
        happyIn8
                 (Rule happy_var_1 happy_var_2 happy_var_6 (Just happy_var_4)
        ) `HappyStk` happyRest}}}}

happyReduce_6 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_6 = happyReduce 7# 3# happyReduction_6
happyReduction_6 (happy_x_7 `HappyStk`
        happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
        case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
        case happyOut11 happy_x_7 of { (HappyWrap11 happy_var_7) -> 
        happyIn8
                 (Rule happy_var_1 happy_var_2 happy_var_7 (Just happy_var_4)
        ) `HappyStk` happyRest}}}}

happyReduce_7 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_7 = happyReduce 4# 3# happyReduction_7
happyReduction_7 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
        case happyOut11 happy_x_4 of { (HappyWrap11 happy_var_4) -> 
        happyIn8
                 (Rule happy_var_1 happy_var_2 happy_var_4 Nothing
        ) `HappyStk` happyRest}}}

happyReduce_8 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_8 = happySpecReduce_3  4# happyReduction_8
happyReduction_8 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
        happyIn9
                 (reverse happy_var_2
        )}

happyReduce_9 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_9 = happySpecReduce_0  4# happyReduction_9
happyReduction_9  =  happyIn9
                 ([]
        )

happyReduce_10 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_10 = happySpecReduce_1  5# happyReduction_10
happyReduction_10 happy_x_1
         =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        happyIn10
                 ([happy_var_1]
        )}

happyReduce_11 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_11 = happySpecReduce_3  5# happyReduction_11
happyReduction_11 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokId) -> 
        happyIn10
                 (happy_var_3 : happy_var_1
        )}}

happyReduce_12 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_12 = happySpecReduce_3  6# happyReduction_12
happyReduction_12 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
        case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
        happyIn11
                 (happy_var_1 : happy_var_3
        )}}

happyReduce_13 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_13 = happySpecReduce_1  6# happyReduction_13
happyReduction_13 happy_x_1
         =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
        happyIn11
                 ([happy_var_1]
        )}

happyReduce_14 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_14 = happyMonadReduce 4# 7# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
        case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
        ( lineP >>= \l -> return (Prod happy_var_1 happy_var_3 l happy_var_2))}}})
        ) (\r -> happyReturn (happyIn12 r))

happyReduce_15 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_15 = happyMonadReduce 3# 7# happyReduction_15
happyReduction_15 (happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
        case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
        ( lineP >>= \l -> return (Prod happy_var_1 happy_var_3 l happy_var_2))}}})
        ) (\r -> happyReturn (happyIn12 r))

happyReduce_16 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_16 = happySpecReduce_1  8# happyReduction_16
happyReduction_16 happy_x_1
         =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        happyIn13
                 (App happy_var_1 []
        )}

happyReduce_17 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_17 = happyReduce 4# 8# happyReduction_17
happyReduction_17 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
        happyIn13
                 (App happy_var_1 (reverse happy_var_3)
        ) `HappyStk` happyRest}}

happyReduce_18 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_18 = happySpecReduce_1  9# happyReduction_18
happyReduction_18 happy_x_1
         =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
        happyIn14
                 (reverse happy_var_1
        )}

happyReduce_19 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_19 = happySpecReduce_0  9# happyReduction_19
happyReduction_19  =  happyIn14
                 ([]
        )

happyReduce_20 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_20 = happySpecReduce_1  10# happyReduction_20
happyReduction_20 happy_x_1
         =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
        happyIn15
                 ([happy_var_1]
        )}

happyReduce_21 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_21 = happySpecReduce_2  10# happyReduction_21
happyReduction_21 happy_x_2
        happy_x_1
         =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
        case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
        happyIn15
                 (happy_var_2 : happy_var_1
        )}}

happyReduce_22 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_22 = happySpecReduce_1  11# happyReduction_22
happyReduction_22 happy_x_1
         =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
        happyIn16
                 ([happy_var_1]
        )}

happyReduce_23 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_23 = happySpecReduce_3  11# happyReduction_23
happyReduction_23 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
        case happyOut13 happy_x_3 of { (HappyWrap13 happy_var_3) -> 
        happyIn16
                 (happy_var_3 : happy_var_1
        )}}

happyReduce_24 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_24 = happySpecReduce_2  12# happyReduction_24
happyReduction_24 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
        happyIn17
                 (PrecId happy_var_2
        )}

happyReduce_25 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_25 = happySpecReduce_1  12# happyReduction_25
happyReduction_25 happy_x_1
         =  happyIn17
                 (PrecShift
        )

happyReduce_26 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_26 = happySpecReduce_0  12# happyReduction_26
happyReduction_26  =  happyIn17
                 (PrecNone
        )

happyReduce_27 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_27 = happySpecReduce_2  13# happyReduction_27
happyReduction_27 happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
        case happyOut19 happy_x_2 of { (HappyWrap19 happy_var_2) -> 
        happyIn18
                 (happy_var_2 : happy_var_1
        )}}

happyReduce_28 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_28 = happySpecReduce_1  13# happyReduction_28
happyReduction_28 happy_x_1
         =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
        happyIn18
                 ([happy_var_1]
        )}

happyReduce_29 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_29 = happySpecReduce_2  14# happyReduction_29
happyReduction_29 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        happyIn19
                 (TokenType happy_var_2
        )}

happyReduce_30 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_30 = happySpecReduce_2  14# happyReduction_30
happyReduction_30 happy_x_2
        happy_x_1
         =  case happyOut21 happy_x_2 of { (HappyWrap21 happy_var_2) -> 
        happyIn19
                 (TokenSpec happy_var_2
        )}

happyReduce_31 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_31 = happySpecReduce_3  14# happyReduction_31
happyReduction_31 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
        case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
        happyIn19
                 (TokenName happy_var_2 happy_var_3 False
        )}}

happyReduce_32 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_32 = happySpecReduce_3  14# happyReduction_32
happyReduction_32 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
        case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
        happyIn19
                 (TokenName happy_var_2 happy_var_3 True
        )}}

happyReduce_33 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_33 = happySpecReduce_1  14# happyReduction_33
happyReduction_33 happy_x_1
         =  happyIn19
                 (TokenImportedIdentity
        )

happyReduce_34 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_34 = happySpecReduce_3  14# happyReduction_34
happyReduction_34 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
        happyIn19
                 (TokenLexer happy_var_2 happy_var_3
        )}}

happyReduce_35 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_35 = happySpecReduce_2  14# happyReduction_35
happyReduction_35 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        happyIn19
                 (TokenMonad "()" happy_var_2 "Prelude.>>=" "Prelude.return"
        )}

happyReduce_36 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_36 = happySpecReduce_3  14# happyReduction_36
happyReduction_36 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
        happyIn19
                 (TokenMonad happy_var_2 happy_var_3 "Prelude.>>=" "Prelude.return"
        )}}

happyReduce_37 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_37 = happyReduce 4# 14# happyReduction_37
happyReduction_37 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
        case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
        happyIn19
                 (TokenMonad "()" happy_var_2 happy_var_3 happy_var_4
        ) `HappyStk` happyRest}}}

happyReduce_38 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_38 = happyReduce 5# 14# happyReduction_38
happyReduction_38 (happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
        case happyOutTok happy_x_4 of { (TokenInfo happy_var_4 TokCodeQuote) -> 
        case happyOutTok happy_x_5 of { (TokenInfo happy_var_5 TokCodeQuote) -> 
        happyIn19
                 (TokenMonad happy_var_2 happy_var_3 happy_var_4 happy_var_5
        ) `HappyStk` happyRest}}}}

happyReduce_39 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_39 = happySpecReduce_2  14# happyReduction_39
happyReduction_39 happy_x_2
        happy_x_1
         =  case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
        happyIn19
                 (TokenNonassoc happy_var_2
        )}

happyReduce_40 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_40 = happySpecReduce_2  14# happyReduction_40
happyReduction_40 happy_x_2
        happy_x_1
         =  case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
        happyIn19
                 (TokenRight happy_var_2
        )}

happyReduce_41 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_41 = happySpecReduce_2  14# happyReduction_41
happyReduction_41 happy_x_2
        happy_x_1
         =  case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
        happyIn19
                 (TokenLeft happy_var_2
        )}

happyReduce_42 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_42 = happySpecReduce_2  14# happyReduction_42
happyReduction_42 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenNum happy_var_2  TokNum) -> 
        happyIn19
                 (TokenExpect happy_var_2
        )}

happyReduce_43 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_43 = happySpecReduce_3  14# happyReduction_43
happyReduction_43 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
        happyIn19
                 (TokenError happy_var_2 happy_var_3
        )}}

happyReduce_44 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_44 = happySpecReduce_1  14# happyReduction_44
happyReduction_44 happy_x_1
         =  happyIn19
                 (TokenErrorExpected
        )

happyReduce_45 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_45 = happySpecReduce_2  14# happyReduction_45
happyReduction_45 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        happyIn19
                 (TokenAttributetype happy_var_2
        )}

happyReduce_46 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_46 = happySpecReduce_3  14# happyReduction_46
happyReduction_46 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokId) -> 
        case happyOutTok happy_x_3 of { (TokenInfo happy_var_3 TokCodeQuote) -> 
        happyIn19
                 (TokenAttribute happy_var_2 happy_var_3
        )}}

happyReduce_47 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_47 = happySpecReduce_1  15# happyReduction_47
happyReduction_47 happy_x_1
         =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        happyIn20
                 (Just happy_var_1
        )}

happyReduce_48 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_48 = happySpecReduce_0  15# happyReduction_48
happyReduction_48  =  happyIn20
                 (Nothing
        )

happyReduce_49 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_49 = happySpecReduce_2  16# happyReduction_49
happyReduction_49 happy_x_2
        happy_x_1
         =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
        case happyOut21 happy_x_2 of { (HappyWrap21 happy_var_2) -> 
        happyIn21
                 (happy_var_1:happy_var_2
        )}}

happyReduce_50 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_50 = happySpecReduce_1  16# happyReduction_50
happyReduction_50 happy_x_1
         =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
        happyIn21
                 ([happy_var_1]
        )}

happyReduce_51 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_51 = happySpecReduce_2  17# happyReduction_51
happyReduction_51 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        case happyOutTok happy_x_2 of { (TokenInfo happy_var_2 TokCodeQuote) -> 
        happyIn22
                 ((happy_var_1, parseTokenSpec happy_var_2)
        )}}

happyReduce_52 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_52 = happySpecReduce_2  18# happyReduction_52
happyReduction_52 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokId) -> 
        case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
        happyIn23
                 (happy_var_1 : happy_var_2
        )}}

happyReduce_53 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_53 = happySpecReduce_0  18# happyReduction_53
happyReduction_53  =  happyIn23
                 ([]
        )

happyReduce_54 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_54 = happySpecReduce_1  19# happyReduction_54
happyReduction_54 happy_x_1
         =  case happyOutTok happy_x_1 of { (TokenInfo happy_var_1 TokCodeQuote) -> 
        happyIn24
                 (Just happy_var_1
        )}

happyReduce_55 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_55 = happySpecReduce_0  19# happyReduction_55
happyReduction_55  =  happyIn24
                 (Nothing
        )

happyTerminalToTok term = case term of {
        TokenEOF -> 30#;
        TokenInfo happy_dollar_dollar TokId -> 2#;
        TokenKW      TokSpecId_TokenType -> 3#;
        TokenKW      TokSpecId_Token -> 4#;
        TokenKW      TokSpecId_Name -> 5#;
        TokenKW      TokSpecId_Partial -> 6#;
        TokenKW      TokSpecId_Lexer -> 7#;
        TokenKW      TokSpecId_ImportedIdentity -> 8#;
        TokenKW      TokSpecId_Monad -> 9#;
        TokenKW      TokSpecId_Nonassoc -> 10#;
        TokenKW      TokSpecId_Left -> 11#;
        TokenKW      TokSpecId_Right -> 12#;
        TokenKW      TokSpecId_Prec -> 13#;
        TokenKW      TokSpecId_Shift -> 14#;
        TokenKW      TokSpecId_Expect -> 15#;
        TokenKW      TokSpecId_Error -> 16#;
        TokenKW      TokSpecId_ErrorExpected -> 17#;
        TokenKW      TokSpecId_Attribute -> 18#;
        TokenKW      TokSpecId_Attributetype -> 19#;
        TokenInfo happy_dollar_dollar TokCodeQuote -> 20#;
        TokenNum happy_dollar_dollar  TokNum -> 21#;
        TokenKW      TokColon -> 22#;
        TokenKW      TokSemiColon -> 23#;
        TokenKW      TokDoubleColon -> 24#;
        TokenKW      TokDoublePercent -> 25#;
        TokenKW      TokBar -> 26#;
        TokenKW      TokParenL -> 27#;
        TokenKW      TokParenR -> 28#;
        TokenKW      TokComma -> 29#;
        _ -> error "Encountered a token that was not declared to happy"
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend kmore = lexTokenP (\tk -> case tk of {
        TokenEOF -> kend tk;
        _ -> kmore (happyTerminalToTok tk) tk })
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 30# tk action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 30# = happyReport'
happyReport _ = happyReport'


happyThen :: () => (P a) -> (a -> (P b)) -> (P b)
happyThen = (Prelude.>>=)
happyReturn :: () => a -> (P a)
happyReturn = (Prelude.return)
happyParse :: () => Happy_GHC_Exts.Int# -> P (HappyAbsSyn )

happyNewToken :: () => Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> (P (HappyAbsSyn ))

happyDoAction :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> (P (HappyAbsSyn ))

happyReduceArr :: () => Happy_Data_Array.Array Prelude.Int (Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> (P (HappyAbsSyn )))

happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyFmap1 f m = happyThen m (\a -> happyReturn (f a))
happyReturn1 :: () => a -> (P a)
happyReturn1 = happyReturn
happyReport' :: () => (Token) -> [Prelude.String] -> (P a) -> (P a)
happyReport' = (\tokens expected resume -> happyError)

happyAbort :: () => (P a)
happyAbort = error "Called abort handler in non-resumptive parser"

ourParser = happySomeParser where
 happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (let {(HappyWrap5 x') = happyOut5 x} in x'))

happySeq = happyDontSeq


happyError :: P a
happyError = failP (\l -> show l ++ ": Parse error\n")
#define HAPPY_COERCE 1
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery
--
-- When there is no applicable action for the current lookahead token `tk`,
-- happy enters error recovery mode. It works in 2 phases:
--
--  1. Fixup: Try to see if there is an action for the error token (`errorTok`,
--     which is ERROR_TOK). If there is, do *not* emit an error and pretend
--     instead that an `errorTok` was inserted.
--     When there is no `errorTok` action, call the error handler
--     (e.g., `happyError`) with the resumption continuation `happyResume`.
--  2. Error resumption mode: TODO: Update!
--     If the error handler wants to resume parsing in
--     order to report multiple parse errors, it will call the resumption
--     continuation (of result type `P (Maybe a)`).
--     In the absence of the %resumptive declaration, this resumption will
--     always (do a bit of work, and) `return Nothing`.
--     In the presence of the %resumptive declaration, the grammar author
--     can use the special `catch` terminal to declare where parsing should
--     resume after an error.
--     E.g., if `stmt : expr ';' | catch ';'` then the resumption will
--
--       (a) Pop off the state stack until it finds an item
--             `stmt -> . catch ';'`.
--           Then, it will push a `catchTok` onto the stack, perform a shift and
--           end up in item `stmt -> catch . ';'`.
--       (b) Discard tokens from the lexer until it finds ';'.
--           (In general, it will discard until the lookahead has a non-default
--           action in the matches a token that applies
--           in the situation `P -> α catch . β`, where β might empty.)
--
-- The `catch` resumption mechanism (2) is what usually is associated with
-- `error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
-- (1) above, we call the corresponding token `catch`.

-- Enter error Fixup: generate an error token,
--                    save the old token and carry on.
--                    When a `happyShift` accepts, we will pop off the error
--                    token to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- parse error if we are in fixup and fail again
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  if happyAlreadyInResumption st sts
    then resume
    else happyReport i tk expected resume

happyAlreadyInResumption st sts
  | (Happy_GHC_Exts.I# n_starts) <- happy_n_starts, LT(st, n_starts)
  = False -- end of the stack
  | (Happy_GHC_Exts.I# st) `elem` happyCatchStates
  = True
  | (HappyCons st1 sts1) <- sts
  = happyAlreadyInResumption st1 sts1

-- happyFail :: Happy_Int -> _ -> Happy_Int -> _
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- happyResume :: Happy_Int -> _ -> Happy_Int -> _
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms - 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    pop_items catch_stacks st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " ++ show (Happy_GHC_Exts.I# st) ++ ", ")
        if null catch_stacks1
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states (reverse " ++ show (map (happy_list_to_list . fst) catch_stacks1) ++ ").\n")
               discard_input_until_exp i tk (reverse catch_stacks1)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_stacks1 st1 sts1 stk1
      where
        !catch_stacks1
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " ++ show (Happy_GHC_Exts.I# st) ++ ", into state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
            null (filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_stacks)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_stacks
          | otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " ++ show (Happy_GHC_Exts.I# st) ++ "\n")
            catch_stacks

    discard_input_until_exp i tk catch_stacks
      | Just (HappyCons st (HappyCons catch_st sts), stk) <- some_catch_state_shifts i catch_stacks
      = DEBUG_TRACE("found expected token in state " ++ show (Happy_GHC_Exts.I# st) ++ " after shifting from " ++ show (Happy_GHC_Exts.I# catch_st) ++ ": " ++ show (Happy_GHC_Exts.I# i) ++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) stk
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | otherwise
      = DEBUG_TRACE("discard token " ++ show (Happy_GHC_Exts.I# i) ++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_stacks) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_stacks)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Nothing
    some_catch_state_shifts i catch_stacks@(((HappyCons st sts),_):_) = try_head i st sts catch_stacks
      where
        try_head i st sts catch_stacks = -- PRECONDITION: head catch_stacks = (HappyCons st sts)
          DEBUG_TRACE("trying token " ++ show (Happy_GHC_Exts.I# i) ++ " in state " ++ show (Happy_GHC_Exts.I# st) ++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (tail catch_stacks)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Just (head catch_stacks)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Just (head catch_stacks)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_stacks

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " ++ show (Happy_GHC_Exts.I# r) ++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " ++ show (Happy_GHC_Exts.I# nt) ++ ", len: " ++ show (Happy_GHC_Exts.I# len) ++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(show (Happy_GHC_Exts.I# new_st) ++ ".\n")
  (HappyCons new_st  sts1)

happyTokenToString :: Prelude.Int -> Prelude.String
happyTokenToString i = happyTokenStrings Prelude.!! (i Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  map happyTokenToString $ search_shifts st sts []
  where
    search_shifts st sts shifts = foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " ++ show (Happy_GHC_Exts.I# st) ++ ", input " ++ show (Happy_GHC_Exts.I# i) ++ ", " ++ show (happyDecodeAction act) ++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Data_List.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
