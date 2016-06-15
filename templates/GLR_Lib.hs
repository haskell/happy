{-# LINE 1 "GLR_Lib.hs" #-}

{-
   GLR_Lib.lhs
   $Id: GLR_Lib.lhs,v 1.5 2005/08/03 13:42:23 paulcc Exp $
-}

 {-
 Parser driver for the GLR parser.

 (c) University of Durham, Ben Medlock 2001
         -- initial code, for structure parsing
 (c) University of Durham, Paul Callaghan 2004-05
         -- extension to semantic rules
         -- shifting to chart data structure
         -- supporting hidden left recursion
         -- many optimisations
 -}

{- supplied by Happy
<> module XYZ (
<>              lexer   -- conditional
-}

        -- probable, but might want to parametrise
           , doParse
           , TreeDecode(..), decode     -- only for tree decode
           , LabelDecode(..)            -- only for label decode

        -- standard exports
           , Tokens
           , GLRResult(..)
           , NodeMap
           , RootNode
           , ForestId
           , GSymbol(..)
           , Branch(..)
           , GSem(..)
           )
  where

import Data.Char
import qualified Data.Map as Map

import Control.Applicative (Applicative(..))
import Control.Monad (foldM, ap)
import Data.Maybe (fromJust)
import Data.List (insertBy, nub, maximumBy, partition, find, groupBy, delete)
#if defined(HAPPY_GHC)
import GHC.Prim
import GHC.Exts
#endif

#if defined(HAPPY_DEBUG)
import System.IO.Unsafe
import Pretty
#endif

{- these inserted by Happy -}

fakeimport DATA

{- borrowed from GenericTemplate.hs -}

#ifdef HAPPY_GHC
#define ILIT(n) n#
#define BANG !
#define IBOX(n) (I# (n))
#define FAST_INT Int#

#if __GLASGOW_HASKELL__ >= 708
#define ULT(n,m) (isTrue# (n <# m))
#define GTE(n,m) (isTrue# (n >=# m))
#define UEQ(n,m) (isTrue# (n ==# m))
#else
#define ULT(n,m) (n <# m)
#define GTE(n,m) (n >=# m)
#define UEQ(n,m) (n ==# m)
#endif

#define PLUS(n,m) (n +# m)
#define MINUS(n,m) (n -# m)
#define TIMES(n,m) (n *# m)
#define NEGATE(n) (negateInt# (n))
#define IF_GHC(x) (x)

#else

#define ILIT(n) (n)
#define BANG
#define IBOX(n) (n)
#define FAST_INT Int
#define ULT(n,m) (n < m)
#define GTE(n,m) (n >= m)
#define UEQ(n,m) (n == m)
#define PLUS(n,m) (n + m)
#define MINUS(n,m) (n - m)
#define TIMES(n,m) (n * m)
#define NEGATE(n) (negate (n))
#define IF_GHC(x)
#endif

#if defined(HAPPY_DEBUG)
#define DEBUG_TRACE(s)    (happyTrace (s) $ return ())
happyTrace string expr = unsafePerformIO $ do
    hPutStr stderr string
    return expr
#else
#define DEBUG_TRACE(s)    {- nothing -}
#endif



doParse = glr_parse


----------------------------------------------------------------------------
-- Main data types

-- A forest is a map of `spans' to branches, where a span is a start position,
-- and end position, and a grammatical category for that interval. Branches
-- are lists of conjunctions of symbols which can be matched in that span.
-- Note that tokens are stored as part of the spans.

type Forest       = Map.Map ForestId [Branch]

---
-- End result of parsing:
--  - successful parse with rooted forest
--  - else syntax error or premature eof

type NodeMap = [(ForestId, [Branch])]
type RootNode = ForestId
type Tokens = [[(Int, GSymbol)]]        -- list of ambiguous lexemes

data GLRResult
 = ParseOK     RootNode Forest    -- forest with root
 | ParseError  Tokens   Forest    -- partial forest with bad input
 | ParseEOF             Forest    -- partial forest (missing input)

-----------------------
-- Forest to simplified output

forestResult :: Int -> Forest -> GLRResult
forestResult length f
 = case roots of
        []       -> ParseEOF f
        [r]      -> ParseOK r f
        rs@(_:_) -> error $ "multiple roots in forest, = " ++ show rs
                                                ++ unlines (map show ns_map)
   where
       ns_map = Map.toList f
       roots  = [ r | (r@(0,sz,sym),_) <- ns_map
                    , sz == length
                    , sym == top_symbol ]


----------------------------------------------------------------------------

glr_parse :: [[UserDefTok]] -> GLRResult
glr_parse toks
 = case runST Map.empty [0..] (tp toks) of
    (f,Left ts)   -> ParseError ts f
                                                -- Error within sentence
    (f,Right ss)  -> forestResult (length toks) f
                                                -- Either good parse or EOF
   where
        tp tss = doActions [initTS 0]
               $ zipWith (\i ts -> [(i, t) | t <- ts]) [0..]
              $ [ [ HappyTok {-j-} t | (j,t) <- zip [0..] ts ] | ts <- tss ]
                ++ [[HappyEOF]]

---

type PM a = ST Forest [Int] a
type FStack = TStack ForestId


---
-- main function

doActions :: [FStack] -> Tokens -> PM (Either Tokens [FStack])

doActions ss []                 -- no more tokens (this is ok)
 = return (Right ss)            -- return the stacks (may be empty)

doActions stks (tok:toks)
 = do
        stkss <- sequence [ do
                             stks' <- reduceAll [] tok_form stks
                             shiftAll tok_form stks'
                         | tok_form <- tok ]
        let new_stks = merge $ concat stkss
        DEBUG_TRACE(unlines $ ("Stacks after R*/S pass" ++ show tok)
                                : map show new_stks)
        case new_stks of            -- did this token kill stacks?
          [] -> case toks of
                  []  -> return $ Right []         -- ok if no more tokens
                  _:_ -> return $ Left (tok:toks)  -- not ok if some input left
          _  -> doActions new_stks toks

reduceAll
 :: [GSymbol] -> (Int, GSymbol) -> [FStack] -> PM [(FStack, Int)]
reduceAll _ tok [] = return []
reduceAll cyclic_names itok@(i,tok) (stk:stks)
 = do
     case action this_state tok of
       Accept      -> reduceAll [] itok stks
       Error       -> reduceAll [] itok stks
       Shift st rs -> do { ss <- redAll rs ; return $ (stk,st) : ss }
       Reduce rs   -> redAll rs
 where
  this_state = top stk
  redAll rs
   = do
        let reds = [ (bf fids,stk',m)
                   | (m,n,bf) <- rs
                   , not (n == 0 && m `elem` cyclic_names)  -- remove done ones
                   , (fids,stk') <- pop n stk
                   ]
                   -- WARNING: incomplete if more than one Empty in a prod(!)
                   -- WARNING: can avoid by splitting emps/non-emps
        DEBUG_TRACE(unlines $ ("Packing reds = " ++ show (length reds))
                            : map show reds)
        stks' <- foldM (pack i) stks reds
        let new_cyclic = [ m | (m,0,_) <- rs
                             , UEQ(this_state, goto this_state m)
                             , m `notElem` cyclic_names ]
        reduceAll (cyclic_names ++ new_cyclic) itok $ merge stks'

shiftAll :: (Int, GSymbol) -> [(FStack, Int)] -> PM [FStack]
shiftAll tok [] = return []
shiftAll (j,tok) stks
 = do
        let end = j + 1
        let key = end `seq` (j,end,tok)
        newNode key
        let mss = [ (stk, st)
                  | ss@((_,st):_) <- groupBy (\a b -> snd a == snd b) stks
                  , stk <- merge $ map fst ss ]
        stks' <- sequence [ do { nid <- getID ; return (push key st nid stk) }
                          | (stk,IBOX(st)) <- mss ]
        return stks'


pack
 :: Int -> [FStack] -> (Branch, FStack, GSymbol) -> PM [FStack]

pack e_i stks (fids,stk,m)
 | ULT(st, ILIT(0))
    = return stks
 | otherwise
    = do
       let s_i = endpoint stk
       let key = (s_i,e_i,m)
       DEBUG_TRACE( unlines
                   $ ("Pack at " ++ show key ++ " " ++ show fids)
                   : ("**" ++ show stk)
                   : map show stks)

       duplicate <- addBranch key fids

       let stack_matches = [ s | s <- stks
                                , UEQ(top s, st)
                               , let (k,s') = case ts_tail s of x:_ -> x
                                , stk == s'
                                , k == key
                                ]  -- look for first obvious packing site
       let appears_in = not $ null stack_matches

       DEBUG_TRACE( unlines
                   $ ("Stack Matches: " ++ show (length stack_matches))
                   : map show stack_matches)
       DEBUG_TRACE( if not (duplicate && appears_in) then "" else
                     unlines
                   $ ("DROP:" ++ show (IBOX(st),key) ++ " -- " ++ show stk)
                   : "*****"
                   : map show stks)

       if duplicate && appears_in
        then return stks       -- because already there
        else do
              nid <- getID
              case stack_matches of
                []  -> return $ insertStack (push key st nid stk) stks
                                -- No prior stacks

                s:_ -> return $ insertStack (push key st nid stk) (delete s stks)
                                -- pack into an existing stack
    where
       st = goto (top stk) m



---
-- record an entry
--  - expected: "i" will contain a token

newNode :: ForestId -> PM ()
newNode i
 = chgS $ \f -> ((), Map.insert i [] f)

---
-- add a new branch
--  - due to packing, we check to see if a branch is already there
--  - return True if the branch is already there

addBranch :: ForestId -> Branch -> PM Bool
addBranch i b
 = do
        f <- useS id
        case Map.lookup i f of
         Nothing               -> chgS $ \f -> (False, Map.insert i [b] f)
         Just bs | b `elem` bs -> return True
                 | otherwise   -> chgS $ \f -> (True,  Map.insert i (b:bs) f)

---
-- only for use with nodes that exist

getBranches ::  ForestId -> PM [Branch]
getBranches i
 = useS $ \s -> Map.findWithDefault no_such_node i s
   where
        no_such_node = error $ "No such node in Forest: " ++ show i





-----------------------------------------------------------------------------
-- Auxiliary functions

(<>) x y = (x,y)  -- syntactic sugar



-- Tomita stack
--  - basic idea taken from Peter Ljungloef's Licentiate thesis


data TStack a
 = TS { top      :: FAST_INT            -- state
      , ts_id    :: FAST_INT            -- ID
      , stoup    :: !(Maybe a)          -- temp holding place, for left rec.
      , ts_tail  :: ![(a,TStack a)]     -- [(element on arc , child)]
      }

instance Show a => Show (TStack a) where
  show ts
   = "St" ++ show (IBOX(top ts))
#if defined(HAPPY_DEBUG)
     ++ "\n" ++ render (spp $ ts_tail ts)
     where
        spp ss = nest 2
                $ vcat [ vcat [text (show (v,IBOX(top s))), spp (ts_tail s)]
                       | (v,s) <- ss ]
#endif


---
-- id uniquely identifies a stack

instance Eq (TStack a) where
      s1 == s2 = UEQ(ts_id s1, ts_id s2)

--instance Ord (TStack a) where
--      s1 `compare` s2 = IBOX(ts_id s1) `compare` IBOX(ts_id s2)

---
-- Nothing special done for insertion
-- - NB merging done at strategic points

insertStack :: TStack a -> [TStack a] -> [TStack a]
insertStack = (:)

---

initTS :: Int -> TStack a
initTS IBOX(id) = TS ILIT(0) id Nothing []

---

push :: ForestId -> FAST_INT -> Int -> TStack ForestId -> TStack ForestId
push x@(s_i,e_i,m) st IBOX(id) stk
 = TS st id stoup [(x,stk)]
   where
        -- only fill stoup for cyclic states that don't consume input
       stoup | s_i == e_i && UEQ(st, goto st m) = Just x
             | otherwise                        = Nothing

---

pop :: Int -> TStack a -> [([a],TStack a)]
pop 0 ts = [([],ts)]
pop 1 st@TS{stoup=Just x}
 = pop 1 st{stoup=Nothing} ++ [ ([x],st) ]
pop n ts = [ (xs ++ [x] , stk')
            | (x,stk) <- ts_tail ts
            , (xs,stk') <- pop (n-1) stk ]

---

popF :: TStack a -> TStack a
popF ts = case ts_tail ts of (_,c):_ -> c

---

endpoint stk
 = case ts_tail stk of
     [] -> 0
     ((_,e_i,_),_):_ -> e_i



---

merge :: (Eq a, Show a) => [TStack a] -> [TStack a]
merge stks
 = [ TS st id ss (nub ch)
   | IBOX(st) <- nub (map (\s -> IBOX(top s)) stks)
   , let ch  = concat  [ x | TS st2 _ _ x <- stks, UEQ(st,st2) ]
         ss  = mkss    [ s | TS st2 _ s _ <- stks, UEQ(st,st2) ]
         (BANG IBOX(id)) = head [ IBOX(i) | TS st2 i _ _ <- stks, UEQ(st,st2) ]
          -- reuse of id is ok, since merge discards old stacks
   ]
   where
        mkss s = case nub [ x | Just x <- s ] of
                   []  -> Nothing
                   [x] -> Just x
                   xs  -> error $ unlines $ ("Stoup merge: " ++ show xs)
                                           : map show stks



----------------------------------------------------------------------------
-- Monad
-- TODO (pcc): combine the s/i, or use the modern libraries - might be faster?
--             but some other things are much, much, much more expensive!

data ST s i a = MkST (s -> i -> (a,s,i))

instance Functor (ST s i) where
 fmap f (MkST sf)
  = MkST $ \s i -> case sf s i of (a,s',i') -> (f a,s',i')

instance Applicative (ST s i) where
 pure a = MkST $ \s i -> (a,s,i)
 (<*>) = ap

instance Monad (ST s i) where
 return = pure
 MkST sf >>= k
  = MkST $ \s i ->
        case sf s i of
         (a,s',i') -> let (MkST sf') = k a in  sf' s' i'

runST :: s -> i -> ST s i a -> (s,a)
runST s i (MkST sf) = case sf s i of
                           (a,s,_) -> (s,a)

chgS :: (s -> (a,s)) -> ST s i a
chgS sf = MkST $ \s i -> let (a,s') = sf s in (a,s',i)

useS :: (s -> b) -> ST s i b
useS fn = MkST $ \s i -> (fn s,s,i)

getID :: ST s [Int] Int
getID = MkST $ \s (i:is) -> (i,s,is)

