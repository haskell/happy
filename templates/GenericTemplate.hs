-- $Id: GenericTemplate.hs,v 1.10 2001/03/30 14:08:23 simonmar Exp $

#ifdef HAPPY_GHC
#define ILIT(n) n#
#define IBOX(n) (I# (n))
#define FAST_INT Int#
#define LT(n,m) (n <# m)
#define GTE(n,m) (n >=# m)
#define EQ(n,m) (n ==# m)
#define PLUS(n,m) (n +# m)
#define MINUS(n,m) (n -# m)
#define TIMES(n,m) (n *# m)
#define NEGATE(n) (negateInt# (n))
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

#if defined(HAPPY_ARRAY)
data Happy_IntList = HappyCons FAST_INT Happy_IntList
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
#define GET_ERROR_TOKEN(x)  (case unsafeCoerce# x of { IBOX(i) -> i })
#define MK_ERROR_TOKEN(i)   (unsafeCoerce# IBOX(i))
#define MK_TOKEN(x)	    (happyInTok (x))
#else
#define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken IBOX(i) -> i })
#define MK_ERROR_TOKEN(i)   (HappyErrorToken IBOX(i))
#define MK_TOKEN(x)	    (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = unsafePerformIO $ do
    hPutStr stderr string
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

happyAccept j tk st sts (HappyStk ans _) = happyReturn1 ans

-----------------------------------------------------------------------------
-- Arrays only: do the next action

#if defined(HAPPY_ARRAY)

happyDoAction i tk st
	= DEBUG_TRACE("state: " ++ show IBOX(st) ++ 
		      ",\ttoken: " ++ show IBOX(i) ++
		      ",\taction: ")
	  case action of
		ILIT(0)		  -> DEBUG_TRACE("fail.\n")
				     happyFail i tk st
		ILIT(-1) 	  -> DEBUG_TRACE("accept.\n")
				     happyAccept i tk st
		n | LT(n,ILIT(0)) -> DEBUG_TRACE("reduce (rule " ++ show rule
						 ++ ")")
				     (happyReduceArr ! rule) i tk st
				     where rule = IBOX(NEGATE(PLUS(n,ILIT(1))))
		n		  -> DEBUG_TRACE("shift, enter state "
						 ++ show IBOX(new_state)
						 ++ "\n")
				     happyShift new_state i tk st
				     where new_state = MINUS(n,ILIT(1))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = PLUS(off,i)
	 check  = if GTE(off_i,ILIT(0)) 
			then EQ(indexShortOffAddr happyCheck off_i, i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

#ifdef HAPPY_GHC
indexShortOffAddr (A# arr) off =
	(i `iShiftL#` 16#) `iShiftRA#` 16#
  where
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#
#else
indexShortOffAddr arr off = arr ! off
#endif

#endif {- HAPPY_ARRAY -}

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
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st@(HAPPYSTATE(action)) sts stk
     = GOTO(action) nt j tk st CONS(st,sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(CONS(st@HAPPYSTATE(action),_)) (v1`HappyStk`stk')
     = GOTO(action) nt j tk st sts (fn v1 `HappyStk` stk')

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _ CONS(_,sts@(CONS(st@HAPPYSTATE(action),_))) (v1`HappyStk`v2`HappyStk`stk')
     = GOTO(action) nt j tk st sts (fn v1 v2 `HappyStk` stk')

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _ CONS(_,CONS(_,sts@(CONS(st@HAPPYSTATE(action),_)))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = GOTO(action) nt j tk st sts (fn v1 v2 v3 `HappyStk` stk')

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk = GOTO(action) nt j tk st1 sts1 (fn stk)
       where sts1@(CONS(st1@HAPPYSTATE(action),_)) = happyDrop k CONS(st,sts)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> GOTO(action) nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(CONS(st1@HAPPYSTATE(action),_)) = happyDrop k CONS(st,sts)
             drop_stk = happyDropStk k stk

happyDrop ILIT(0) l = l
happyDrop n CONS(_,t) = happyDrop MINUS(n,ILIT(1)) t

happyDropStk ILIT(0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk MINUS(n,ILIT(1)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

#if defined(HAPPY_ARRAY)
happyGoto nt j tk st = 
   DEBUG_TRACE(", goto state " ++ show IBOX(new_state) ++ "\n")
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = PLUS(off,nt)
 	 new_state = indexShortOffAddr happyTable off_i
#else
happyGoto action j tk st = action j j tk (HappyState action)
#endif

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail  ERROR_TOK tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk HAPPYSTATE(action) sts stk =
--      trace "entering error recovery" $
	DO_ACTION(action,ERROR_TOK,tk,sts, MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

#if defined(HAPPY_GHC)
happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}
#endif

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
