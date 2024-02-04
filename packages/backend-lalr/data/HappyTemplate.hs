-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#ifdef HAPPY_GHC
#  if !defined(__GLASGOW_HASKELL__)
#    error `HAPPY_GHC` is defined but this code isn't being built with GHC.
#  endif
#  define ILIT(n) n#
#  define IBOX(n) (Happy_GHC_Exts.I# (n))
#  define FAST_INT Happy_GHC_Exts.Int#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#  if __GLASGOW_HASKELL__ > 706
#    define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#    define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#    define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#  else
#    define LT(n,m) (n Happy_GHC_Exts.<# m)
#    define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#    define EQ(n,m) (n Happy_GHC_Exts.==# m)
#  endif
#  define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#  define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#  define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#  define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))
#  define IF_GHC(x) (x)
#else
#  define ILIT(n) (n)
#  define IBOX(n) (n)
#  define FAST_INT Prelude.Int
#  define LT(n,m) (n Prelude.< m)
#  define GTE(n,m) (n Prelude.>= m)
#  define EQ(n,m) (n Prelude.== m)
#  define PLUS(n,m) (n Prelude.+ m)
#  define MINUS(n,m) (n Prelude.- m)
#  define TIMES(n,m) (n Prelude.* m)
#  define NEGATE(n) (Prelude.negate (n))
#  define IF_GHC(x)
#endif

data Happy_IntList = HappyCons FAST_INT Happy_IntList

#if defined(HAPPY_ARRAY)
#  define CONS(h,t) (HappyCons (h) (t))
#else
#  define CONS(h,t) ((h):(t))
#endif

#if defined(HAPPY_ARRAY)
#  define ERROR_TOK ILIT(0)
#  define CATCH_TOK ILIT(1)
#  define DO_ACTION(state,i,tk,sts,stk) happyDoAction i tk state sts (stk)
#  define HAPPYSTATE(i) (i)
#  define GOTO(action) happyGoto
#  define IF_ARRAY(x) (x)
#else
#  define ERROR_TOK ILIT(1)
#  define CATCH_TOK ILIT(2)
#  define DO_ACTION(state,i,tk,sts,stk) state i i tk HAPPYSTATE(state) sts (stk)
#  define HAPPYSTATE(i) (HappyState (i))
#  define GOTO(action) action
#  define IF_ARRAY(x)
#endif

#if defined(HAPPY_COERCE)
#  if !defined(HAPPY_GHC)
#    error `HAPPY_COERCE` requires `HAPPY_GHC`
#  endif
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { IBOX(i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# IBOX(i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken IBOX(i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken IBOX(i))
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
data HappyStk a = HappyStk !a (HappyStk a)

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
        IF_GHC(happyTcHack j IF_ARRAY(happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

#if defined(HAPPY_ARRAY)

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show IBOX(st) ++
              ",\ttoken: " ++ show IBOX(i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail   -> DEBUG_TRACE("failing.\n")
                   happyFail i tk st
    HappyAccept -> DEBUG_TRACE("accept.\n")
                   happyAccept i tk st
    HappyReduce rule -> DEBUG_TRACE("reduce (rule " ++ show IBOX(rule) ++ ")")
                        (happyReduceArr Happy_Data_Array.! IBOX(rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show IBOX(new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (IBOX(act)) -> act
  Nothing          -> indexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off,ILIT(0)), EQ(indexOffAddr happyCheck off, i)
  = Prelude.Just (IBOX(indexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(indexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce FAST_INT -- rule number
  | HappyShift FAST_INT  -- new state
  deriving Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction ILIT(0)  = HappyFail
happyDecodeAction ILIT(-1) = HappyAccept
happyDecodeAction action
  | LT(action,ILIT(0))
  = HappyReduce NEGATE(PLUS(action,ILIT(1)))
  | otherwise
  = HappyShift MINUS(action,ILIT(1))

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = indexOffAddr happyTable off
  where
    off = PLUS(indexOffAddr happyGotoOffsets st, nt)

#endif /* HAPPY_ARRAY */

#ifdef HAPPY_GHC
indexOffAddr (HappyA# arr) off =
  Happy_GHC_Exts.int32ToInt# (Happy_GHC_Exts.indexInt32OffAddr# arr off)

#ifdef HAPPY_ARRAY
indexRuleArr arr r = (IBOX(nt), IBOX(len))
  where
    IBOX(n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),ILIT(2))
    nt = indexOffAddr arr offs
    len = indexOffAddr arr PLUS(offs,ILIT(1))
#endif
#else
indexOffAddr arr off = arr Happy_Data_Array.! off

#ifdef HAPPY_ARRAY
indexRuleArr arr nt = arr Happy_Data_Array.! nt
#endif
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

#ifdef HAPPY_GHC
readArrayBit arr bit =
    Bits.testBit IBOX(indexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#)) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x
#else
readArrayBit arr bit =
    Bits.testBit IBOX(indexOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)
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
     DEBUG_TRACE("shifting the error token")
     DO_ACTION(new_state,i,tk,CONS(st,sts),stk)
happyShift new_state i tk st sts stk =
     happyNewToken new_state CONS(st,sts) (MK_TOKEN(tk)`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st@(HAPPYSTATE(action)) sts stk
     = happySeq fn (GOTO(action) nt j tk st CONS(st,sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(CONS(st@HAPPYSTATE(action),_)) (v1`HappyStk`stk')
     = let r = fn v1 in
       IF_ARRAY(happyTcHack old_st) happySeq r (GOTO(action) nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 nt fn j tk old_st CONS(_,sts@(CONS(st@HAPPYSTATE(action),_))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       IF_ARRAY(happyTcHack old_st) happySeq r (GOTO(action) nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 nt fn j tk old_st CONS(_,CONS(_,sts@(CONS(st@HAPPYSTATE(action),_)))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       IF_ARRAY(happyTcHack old_st) happySeq r (GOTO(action) nt j tk st sts (r `HappyStk` stk'))

happyReduce k nt fn j tk st sts stk =
      case happyDrop k CONS(st,sts) of
         sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (GOTO(action) nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k CONS(st,sts) of
        sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> GOTO(action) nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      j `happyTcHack` case happyDrop k CONS(st,sts) of
        sts1@(CONS(st1@HAPPYSTATE(action),_)) ->
         let drop_stk = happyDropStk k stk
#if defined(HAPPY_ARRAY)
             new_state = happyIndexGotoTable nt st1
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
   happyDoAction j tk new_state where new_state = (happyIndexGotoTable nt st)
#else
happyGoto action j tk st = action j j tk (HappyState action)
#endif

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
--  2. Error resumption mode: If the error handler wants to resume parsing in
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
happyTryFixup i tk HAPPYSTATE(action) sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  DO_ACTION(action,ERROR_TOK,tk,sts, MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- parse error if we are in fixup and fail again
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
#if defined(HAPPY_ARRAY)
  let resume   = happyResume i tk st sts stk
      expected = map happyTokenToString (happyExpectedTokens st sts) in
  if happyAlreadyInResumption st sts
    then resume
    else happyReport i tk expected resume

happyAlreadyInResumption st sts
  | IBOX(n_starts) <- happy_n_starts, LT(st, n_starts)
  = False -- end of the stack
  | IBOX(st) `elem` happyCatchStates
  = True
  | CONS(st1,sts1) <- sts
  = happyAlreadyInResumption st1 sts1
#else
  happyReport i tk [] happyAbort
#endif

happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

#if defined(HAPPY_ARRAY)
happyResume i tk st sts stk = pop_items st sts stk
  where
    pop_items st sts stk
      | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
      = DEBUG_TRACE("shifting catch token " ++ show IBOX(st) ++ " -> " ++ show IBOX(new_state) ++ "\n")
        discard_input_until_exp i tk new_state CONS(st,sts) (MK_ERROR_TOKEN(i) `HappyStk` stk)
      | DEBUG_TRACE("can't shift catch in " ++ show IBOX(st) ++ ", ") True
      , IBOX(n_starts) <- happy_n_starts, LT(st, n_starts)
      = DEBUG_TRACE("because it is a start state. no resumption.\n")
        happyAbort
      | CONS(st1,sts1) <- sts, _ `HappyStk` stk1 <- stk
      = DEBUG_TRACE("discarding.\n")
        pop_items st1 sts1 stk1
    discard_input_until_exp i tk st sts stk
      | ultimately_fails i st sts
      = DEBUG_TRACE("discard token in state " ++ show IBOX(st) ++ ": " ++ show IBOX(i) ++ "\n")
        happyLex (\_eof_tk -> happyAbort)
                 (\i tk -> discard_input_until_exp i tk st sts stk) -- not eof
      | otherwise
      = DEBUG_TRACE("found expected token in state " ++ show IBOX(st) ++ ": " ++ show IBOX(i) ++ "\n")
        DO_ACTION(st,i,tk,sts,stk)
    ultimately_fails i st sts =
      DEBUG_TRACE("trying token " ++ show IBOX(i) ++ " in state " ++ show IBOX(st) ++ ": ")
      case happyDecodeAction (happyNextAction i st) of
        HappyFail     -> DEBUG_TRACE("fail.\n")   True
        HappyAccept   -> DEBUG_TRACE("accept.\n") False
        HappyShift _  -> DEBUG_TRACE("shift.\n")  False
        HappyReduce r -> case happySimulateReduce r st sts of
          CONS(st1,sts1) -> ultimately_fails i st1 sts1

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " ++ show IBOX(r) ++ ", ")
  let (IBOX(nt), IBOX(len)) = indexRuleArr happyRuleArr r in
  DEBUG_TRACE("nt " ++ show IBOX(nt) ++ ", len: " ++ show IBOX(len) ++ ", new_st ")
  let sts1@CONS(st1,_) = happyDrop len CONS(st,sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(show IBOX(new_st) ++ ".\n")
  CONS(new_st, sts1)

happyTokenToString i = happyTokenStrings Prelude.!! (i Prelude.- 2)
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  search_shifts st sts []
  where
    search_shifts st sts shifts = foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (IBOX(i), IBOX(act)) shifts =
      DEBUG_TRACE("found action in state " ++ show IBOX(st) ++ ", input " ++ show IBOX(i) ++ ", " ++ show (happyDecodeAction act) ++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Data_List.insert IBOX(i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          CONS(st1,sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      = ((-1), IBOX(indexOffAddr happyDefActions st))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = indexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off IBOX(i)
      | let off_i = PLUS(off,i)
      , GTE(off_i,ILIT(0))
      , EQ(indexOffAddr happyCheck off_i,i)
      = [IBOX(indexOffAddr happyTable off_i)]
      | otherwise
      = []

#endif


-- Internal happy errors:

#if defined(HAPPY_GHC)
notHappyAtAll :: Happy_GHC_Stack.HasCallStack => a
#else
notHappyAtAll :: a
#endif
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

#if defined(HAPPY_GHC)
happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}
#else
happyTcHack x y = y
#endif

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
