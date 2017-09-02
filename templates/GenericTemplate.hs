-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#ifdef HAPPY_GHC
#undef __GLASGOW_HASKELL__
#define HAPPY_IF_GHC_GT_500 #if __GLASGOW_HASKELL__ > 500
#define HAPPY_IF_GHC_GE_503 #if __GLASGOW_HASKELL__ >= 503
#define HAPPY_ELIF_GHC_500 #elif __GLASGOW_HASKELL__ == 500
#define HAPPY_IF_GHC_GT_706 #if __GLASGOW_HASKELL__ > 706
#define HAPPY_ELSE #else
#define HAPPY_ENDIF #endif
#define HAPPY_DEFINE #define
#endif

#ifdef HAPPY_GHC
#define ILIT(n) n#
#define IBOX(n) (Happy_GHC_Exts.I# (n))
#define FAST_INT Happy_GHC_Exts.Int#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
HAPPY_IF_GHC_GT_706
HAPPY_DEFINE LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
HAPPY_DEFINE GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
HAPPY_DEFINE EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
HAPPY_ELSE
HAPPY_DEFINE LT(n,m) (n Happy_GHC_Exts.<# m)
HAPPY_DEFINE GTE(n,m) (n Happy_GHC_Exts.>=# m)
HAPPY_DEFINE EQ(n,m) (n Happy_GHC_Exts.==# m)
HAPPY_ENDIF
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))
#define IF_GHC(x) (x)
#else
#define ILIT(n) (n)
#define IBOX(n) (n)
#define FAST_INT Int
#define LT(n,m) (n < m)
#define GTE(n,m) (n >= m)
#define EQ(n,m) (n == m)
#define PLUS(n,m) (n + m)
#define MINUS(n,m) (n - m)
#define TIMES(n,m) (n * m)
#define NEGATE(n) (negate (n))
#define IF_GHC(x)
#endif

data Happy_IntList = HappyCons FAST_INT Happy_IntList

#if defined(HAPPY_ARRAY)
#define CONS(h,t) (HappyCons (h) (t))
#else
#define CONS(h,t) ((h):(t))
#endif

#if defined(HAPPY_ARRAY)
#define ERROR_TOK ILIT(0)
#define DO_ACTION(state,i,tk,sts,stk) happyDoAction i tk state sts (stk)
#define HAPPYSTATE(i) (i)
#define GOTO(action) happyGoto
#define IF_ARRAYS(x) (x)
#else
#define ERROR_TOK ILIT(1)
#define DO_ACTION(state,i,tk,sts,stk) state i i tk HAPPYSTATE(state) sts (stk)
#define HAPPYSTATE(i) (HappyState (i))
#define GOTO(action) action
#define IF_ARRAYS(x) 
#endif

#if defined(HAPPY_COERCE)
#define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { IBOX(i) -> i })
#define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# IBOX(i))
#define MK_TOKEN(x)         (happyInTok (x))
#else
#define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken IBOX(i) -> i })
#define MK_ERROR_TOKEN(i)   (HappyErrorToken IBOX(i))
#define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#define DEBUG_TRACE(s)    {- nothing -}
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
        IF_GHC(happyTcHack j IF_ARRAYS(happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

#if defined(HAPPY_ARRAY)

happyDoAction i tk st
        = DEBUG_TRACE("state: " ++ show IBOX(st) ++ 
                      ",\ttoken: " ++ show IBOX(i) ++
                      ",\taction: ")
          case action of
                ILIT(0)           -> DEBUG_TRACE("fail.\n")
                                     happyFail (happyExpListPerState (IBOX(st) :: Int)) i tk st
                ILIT(-1)          -> DEBUG_TRACE("accept.\n")
                                     happyAccept i tk st
                n | LT(n,(ILIT(0) :: FAST_INT)) -> DEBUG_TRACE("reduce (rule " ++ show rule
                                                               ++ ")")
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = IBOX(NEGATE(PLUS(n,(ILIT(1) :: FAST_INT))))
                n                 -> DEBUG_TRACE("shift, enter state "
                                                 ++ show IBOX(new_state)
                                                 ++ "\n")
                                     happyShift new_state i tk st
                                     where new_state = MINUS(n,(ILIT(1) :: FAST_INT))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = PLUS(off,i)
         check  = if GTE(off_i,(ILIT(0) :: FAST_INT))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

#endif /* HAPPY_ARRAY */

#ifdef HAPPY_GHC
indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#
#else
indexShortOffAddr arr off = arr Happy_Data_Array.! off
#endif


#ifdef HAPPY_GHC
readArrayBit arr bit =
    Bits.testBit IBOX(indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#)) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x
#else
readArrayBit arr bit =
    Bits.testBit IBOX(indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)
#endif

#ifdef HAPPY_GHC
data HappyAddr = HappyA# Happy_GHC_Exts.Addr#
#endif

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

#if !defined(HAPPY_ARRAY)

newtype HappyState b c = HappyState
        (FAST_INT ->                    -- token number
         FAST_INT ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)

#endif

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--     trace "shifting the error token" $
     DO_ACTION(new_state,i,tk,CONS(st,sts),stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state CONS(st,sts) (MK_TOKEN(tk)`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st@(HAPPYSTATE(action)) sts stk
     = GOTO(action) nt j tk st CONS(st,sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(CONS(st@HAPPYSTATE(action),_)) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (GOTO(action) nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _ CONS(_,sts@(CONS(st@HAPPYSTATE(action),_))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (GOTO(action) nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _ CONS(_,CONS(_,sts@(CONS(st@HAPPYSTATE(action),_)))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (GOTO(action) nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(ILIT(1) :: FAST_INT)) sts of
         sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (GOTO(action) nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k CONS(st,sts) of
        sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> GOTO(action) nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k CONS(st,sts) of
        sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
         let drop_stk = happyDropStk k stk
#if defined(HAPPY_ARRAY)
             off = indexShortOffAddr happyGotoOffsets st1
             off_i = PLUS(off,nt)
             new_state = indexShortOffAddr happyTable off_i
#else
             _ = nt :: FAST_INT
             new_state = action
#endif
          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop ILIT(0) l = l
happyDrop n CONS(_,t) = happyDrop MINUS(n,(ILIT(1) :: FAST_INT)) t

happyDropStk ILIT(0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk MINUS(n,(ILIT(1)::FAST_INT)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

#if defined(HAPPY_ARRAY)
happyGoto nt j tk st = 
   DEBUG_TRACE(", goto state " ++ show IBOX(new_state) ++ "\n")
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = PLUS(off,nt)
         new_state = indexShortOffAddr happyTable off_i
#else
happyGoto action j tk st = action j j tk (HappyState action)
#endif

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk HAPPYSTATE(action) sts stk =
--      trace "entering error recovery" $
        DO_ACTION(action,ERROR_TOK,tk,sts, MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

#if defined(HAPPY_GHC)
happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}
#endif

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

#if defined(HAPPY_ARRAY)
{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}
#endif
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
