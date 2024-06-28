-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
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
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
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
  deriving Show

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

indexRuleArr arr r = (Happy_GHC_Exts.I# nt, Happy_GHC_Exts.I# len)
  where
    (Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr arr offs
    len = happyIndexOffAddr arr PLUS(offs,1#)

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk
-- TODO: When `i` would enter error recovery again, we should instead
-- discard input until the lookahead is acceptable. Perhaps this is
-- simplest to implement in CodeGen for productions using `error`;
-- there we know the context and can implement local shift+discard actions.
-- still need to remember parser-defined error site, though.

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
     = case happyDrop k (HappyCons st sts) of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      j `happyTcHack` case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              new_state = happyIndexGotoTable nt st1
          in
            happyThen1 (fn stk tk)
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
      expected = map happyTokenToString (happyExpectedTokens st sts) in
    if happyAlreadyInResumption st sts
    then resume
    else happyReport i tk expected resume

happyAlreadyInResumption st sts
  | (Happy_GHC_Exts.I# n_starts) <- happy_n_starts, LT(st, n_starts)
  = False -- end of the stack
  | (Happy_GHC_Exts.I# st) `elem` happyCatchStates
  = True
  | HappyCons st1 sts1 <- sts
  = happyAlreadyInResumption st1 sts1

happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

happyResume i tk st sts stk = pop_items st sts stk
  where
    pop_items st sts stk
      | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
      = DEBUG_TRACE("shifting catch token " ++ show (Happy_GHC_Exts.I# st)
                    ++ " -> " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
        discard_input_until_exp i tk new_state (HappyCons st sts) (MK_ERROR_TOKEN(i) `HappyStk` stk)
      | DEBUG_TRACE("can't shift catch in " ++ show (Happy_GHC_Exts.I# st) ++ ", ") True
      , (Happy_GHC_Exts.I# n_starts) <- happy_n_starts, LT(st, n_starts)
      = DEBUG_TRACE("because it is a start state. no resumption.\n")
        happyAbort
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = DEBUG_TRACE("discarding.\n")
        pop_items st1 sts1 stk1
    discard_input_until_exp i tk st sts stk
      | ultimately_fails i st sts
      = DEBUG_TRACE("discard token in state " ++ show (Happy_GHC_Exts.I# st)
                    ++ ": " ++ show (Happy_GHC_Exts.I# i) ++ "\n")
        happyLex (\_eof_tk -> happyAbort)
                 (\i tk -> discard_input_until_exp i tk st sts stk) -- not eof
      | otherwise
      = DEBUG_TRACE("found expected token in state " ++ show (Happy_GHC_Exts.I# st)
                    ++ ": " ++ show (Happy_GHC_Exts.I# i) ++ "\n")
        (happyDoAction i tk st sts stk)

    ultimately_fails i st sts =
      DEBUG_TRACE("trying token " ++ show (Happy_GHC_Exts.I# i)
                  ++ " in state " ++ show (Happy_GHC_Exts.I# st) ++ ": ")
      case happyDecodeAction (happyNextAction i st) of
        HappyFail     -> DEBUG_TRACE("fail.\n")   True
        HappyAccept   -> DEBUG_TRACE("accept.\n") False
        HappyShift _  -> DEBUG_TRACE("shift.\n")  False
        HappyReduce r -> case happySimulateReduce r st sts of
                           HappyCons st1 sts1 -> ultimately_fails i st1 sts1

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " ++ show (Happy_GHC_Exts.I# r) ++ ", ")
  let (Happy_GHC_Exts.I# nt, Happy_GHC_Exts.I# len) = indexRuleArr happyRuleArr r in
  DEBUG_TRACE("nt " ++ show (Happy_GHC_Exts.I# nt) ++ ", len: "
              ++ show (Happy_GHC_Exts.I# len) ++ ", new_st ")
  let sts1@(HappyCons st1 _) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(show (Happy_GHC_Exts.I# new_st) ++ ".\n")
  HappyCons new_st sts1

happyTokenToString i = happyTokenStrings Prelude.!! (i Prelude.- 2)
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  search_shifts st sts []
  where
    search_shifts st sts shifts = foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " ++ show (Happy_GHC_Exts.I# st)
                  ++ ", input " ++ show (Happy_GHC_Exts.I# i) ++ ", "
                  ++ show (happyDecodeAction act) ++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Data_List.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
                           HappyCons st1 sts1 -> search_shifts st1 sts1 shifts
    distinct_actions st
      = ((-1), Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i)
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i)]
      | otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

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
