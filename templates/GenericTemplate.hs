-- $Id: GenericTemplate.hs,v 1.4 1999/10/11 17:13:57 simonmar Exp $

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
data Happy_IntList = HappyNil | HappyCons FAST_INT Happy_IntList
#define NIL HappyNil
#define CONS(h,t) (HappyCons (h) (t))
#else
#define NIL []
#define CONS(h,t) ((h):(t))
#endif

#if defined(HAPPY_ARRAY)
#define ACTION_0 ILIT(0)
#define ERROR_TOK ILIT(0)
#define DO_ACTION(state,i,tk,sts,stk) happyDoAction i tk state sts (stk)
#define HAPPYSTATE(i) (i)
#define GOTO(action) happyGoto
#define IF_ARRAYS(x) (x)
#else
#define ACTION_0 action_0
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
#define HAPPY_FINAL_ABSSYN(x) (happyOut4 (x))
#else
#define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken IBOX(i) -> i })
#define MK_ERROR_TOKEN(i)   (HappyErrorToken IBOX(i))
#define MK_TOKEN(x)	    (HappyTerminal (x))
#define HAPPY_FINAL_ABSSYN(x) (case (x) of {HappyAbsSyn4 z -> z })
#endif

#if defined(HAPPY_DEBUG)
#define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = unsafePerformIO $ do
    hPutStr stderr string
    return expr
#else
#define DEBUG_TRACE(s)    {- nothing -}
#endif

-----------------------------------------------------------------------------
-- starting the parse

happyParse = happyNewToken ACTION_0 NIL []

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
-- Accepting the parse

happyAccept j tk st sts [ ans ] = happyReturn HAPPY_FINAL_ABSSYN(ans)
happyAccept j tk st sts _       = IF_GHC(happyTcHack j 
				         IF_ARRAYS(happyTcHack st))
				  notHappyAtAll

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x : _) =
     let i = GET_ERROR_TOKEN(x) in
--     trace "shifting the error token" $
     DO_ACTION(new_state,i,tk,CONS(st,sts),stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state CONS(st,sts) (MK_TOKEN(tk):stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st@(HAPPYSTATE(action)) sts stk
     = GOTO(action) nt j tk st CONS(st,sts) (fn : stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(CONS(st@HAPPYSTATE(action),_)) (v1:stk')
     = GOTO(action) nt j tk st sts (fn v1 : stk')
happySpecReduce_1 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _ CONS(_,sts@(CONS(st@HAPPYSTATE(action),_))) (v1:v2:stk')
     = GOTO(action) nt j tk st sts (fn v1 v2 : stk')
happySpecReduce_2 _ _ _ _ _ _ _
     = notHappyAtAll

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _ CONS(_,CONS(_,sts@(CONS(st@HAPPYSTATE(action),_)))) (v1:v2:v3:stk')
     = GOTO(action) nt j tk st sts (fn v1 v2 v3 : stk')
happySpecReduce_3 _ _ _ _ _ _ _
     = notHappyAtAll

happyRedcue k i fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk = GOTO(action) nt j tk st1 sts1 (fn stk)
       where sts1@(CONS(st1@HAPPYSTATE(action),_)) = happyDrop k CONS(st,sts)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen (fn stk) (\r -> GOTO(action) nt j tk st1 sts1 (r : drop_stk))
       where sts1@(CONS(st1@HAPPYSTATE(action),_)) = happyDrop k CONS(st,sts)
             drop_stk = drop IBOX(k) stk

happyDrop ILIT(0) l = l
happyDrop n CONS(_,t) = happyDrop MINUS(n,ILIT(1)) t

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

#if defined(HAPPY_ARRAY)
-- subtract 4 from nt, because nonterminals start at 4 (see Grammar.hs)
happyGoto nt j tk st = 
   DEBUG_TRACE(", goto state " ++ show IBOX(new_state) ++ "\n")
   happyDoAction j tk new_state
   where i      = MINUS(nt,ILIT(4))
	 off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = PLUS(off,i)
 	 new_state = indexShortOffAddr happyTable off_i
#else
happyGoto action j tk st = action j j tk (HappyState action)
#endif

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail  ERROR_TOK tk old_st NIL stk =
--	trace "failing" $ 
    	happyError

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
						(saved_tok : _ : stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok:stk))

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk HAPPYSTATE(action) sts stk =
--      trace "entering error recovery" $
	DO_ACTION(action,ERROR_TOK,tk,sts, MK_ERROR_TOKEN(i) : stk)

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