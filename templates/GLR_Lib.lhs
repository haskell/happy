>{-# LINE 1 "GLR_Lib.lhs" #-}

{-
   GLR_Lib.lhs
   $Id: GLR_Lib.lhs,v 1.1 2004/08/11 15:39:32 paulcc Exp $
-}

Parser driver for the GLR parser.

(c) University of Durham, Ben Medlock 2001
        -- initial code, for structure parsing
(c) University of Durham, Paul Callaghan 2004
        -- extension to semantic rules, and various optimisations

{- supplied by Happy 
<> module XYZ ( 
<>              lexer	-- conditional
-}

>	-- probable, but might want to parametrise
>           , doParse
>           , TreeDecode(..), decode	-- only for tree decode
>           , LabelDecode(..)		-- only for label decode

>	-- standard exports
>           , GLRResult(..)
>           , Tokens
>           , ForestNode(..)
>           , NodeMap
>           , Branch(..)
>           , GSymbol(..)
>           , GSem(..)
>           )
>  where

<> import IOExts 

>import Char
>import System
>import Data.FiniteMap

>import Monad (foldM)
>import Maybe ( fromJust )
>import List ( insertBy , nub , maximumBy, partition , find )

>{- these inserted by Happy -}

>import DATA
>-- import GHC.Exts

{- borrowed from GenericTemplate.hs -}

#ifdef HAPPY_GHC
#define ILIT(n) n#
#define IBOX(n) (I# (n))
#define FAST_INT Int#
#define ULT(n,m) (n <# m)
#define GTE(n,m) (n >=# m)
#define UEQ(n,m) (n ==# m)
#define PLUS(n,m) (n +# m)
#define MINUS(n,m) (n -# m)
#define TIMES(n,m) (n *# m)
#define NEGATE(n) (negateInt# (n))
#define IF_GHC(x) (x)
#else
#define ILIT(n) (n)
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


>doParse = glr_parse


%----------------------------------------------------------------------------
Main data types

A forest is a bag of nodes, effectively providing a reference-counted
mapping from a forest index (an Int) to a node. A forest node contains
a start position, and end position, a branch label (a non-terminal) and
a list of branches which have been matched at that position and under that
non-terminal. 

>type Forest       = BagMap ForestNode
>data ForestNode   = FNode !Int !Int !GSymbol ![Branch] deriving (Show)

---
Equality and Ordering on ForestNodes
 - A node is fully determined by the category, the span, and the children
   it spans - so we have to check all of these when doing the packing
 - There's no specific Equality on Branches - it is all done here, manually

>instance Eq ForestNode where
>	FNode x1 x2 xs xbs == FNode y1 y2 ys ybs 
>	 = x1 == y1 && x2 == y2 && xs == ys 
>	   && map b_nodes xbs == map b_nodes ybs

>instance Ord ForestNode where
>	FNode x1 x2 xs xbs `compare` FNode y1 y2 ys ybs 
>	 = case x1 `compare` y1 of
>	     EQ -> case x2 `compare` y2 of 
>	             EQ -> case xs `compare` ys of
>	                     EQ -> map b_nodes xbs `compare` map b_nodes ybs
>	                     ne -> ne
>	             ne -> ne
>	     ne -> ne


---
End result of parsing: 
 - successful parse with rooted forest
 - else syntax error or premature eof

>type NodeMap = [(ForestId, ForestNode)]
>type RootNode = ForestId
>type Tokens = [[(Int, GSymbol)]]	-- list of ambiguous lexemes

>data GLRResult 
> = ParseOK     RootNode NodeMap    -- forest with root
> | ParseError  Tokens   NodeMap    -- partial forest with bad input
> | ParseEOF             NodeMap    -- partial forest (missing input)
>   deriving Show

%-----------------------
Forest to simplified output

>forestResult :: Int -> Forest -> GLRResult
>forestResult length f
> = case roots of
>	[]       -> ParseEOF ns_map
>	[r]      -> ParseOK r ns_map
>	rs@(_:_) -> error $ "multiple roots in forest, = " ++ show rs
>						++ unlines (map show ns_map)
>   where
>	ns_map = assocs f 
>	roots = [ r | (r, FNode 0 sz sym _) <- ns_map
>	            , sz == length
>	            , sym == top_symbol ]
>	-- children = concat [ concatMap b_nodes bs 
>      --                   | FNode _ _ _ bs <- map snd ns_map ]
>	-- roots = [ n | n <- map fst ns_map, n `notElem` children ]


%----------------------------------------------------------------------------

>glr_parse :: [[UserDefTok]] -> GLRResult
>glr_parse toks 
> = case runST initSM [0..] (tp toks) of
>    (f,Left ts )   -> ParseError ts (assocs f) 
>						-- Error within sentence
>    (f,Right ss )  -> forestResult (length toks) f
>						-- Either good parse or EOF
>   where
>	tp tss = doActions [initTS 0] 
>	       $ zipWith (\i ts -> [(i, t) | t <- ts]) [0..] 
>             $ map (map HappyTok) tss ++ [[HappyEOF]]

---

>type PM a = ST (BagMap ForestNode) [Int] a

>doActions :: [TStack Int] -> Tokens -> PM (Either Tokens [TStack Int])

>doActions ss [] 		-- no more tokens (this is ok)
> = return (Right ss)		-- return the stacks (may be empty)

>doActions stks (tok:toks)
> = do
>	stkss <- sequence [ reduceAll tok_form stks >>= shiftAll tok_form
>                        | tok_form <- tok ]
>	case merge $ concat stkss of 		-- did this token kill stacks?
>	  [] -> case toks of
>		  []  -> return $ Right []	   -- ok if no more tokens
>		  _:_ -> return $ Left (tok:toks)  -- not ok if some input left
>	  ss -> doActions ss toks

>reduceAll :: (Int, GSymbol) -> [TStack Int] -> PM [(TStack Int, Int)]
>reduceAll tok [] = return []
>reduceAll itok@(i,tok) (stk:stks)
> = case action (top stk) tok of
>    Accept	  -> reduceAll itok stks
>    Error	  -> -- trace ("Clash @ " ++ show (itok, top stk) ++ "\n")
>	             -- DISCARDS current stack - can't continue 
>                   -- should really do GC in BagMap for discarded items
>	             reduceAll itok stks
>    Shift st rs -> do { ss <- redAll rs ; return $ (stk,st):ss } 
>    Reduce rs   -> redAll rs
> where 
>  redAll rs = do 
>		   let reds = concatMap (reduce stk) rs 
>		   stks' <- foldM (pack i) stks reds
>		   reduceAll itok stks' 
>  reduce stk (m,n,bf) = [ (bf fids,stk',m) | (fids,stk') <- pop n stk ]


>shiftAll :: (Int, GSymbol) -> [(TStack Int, Int)] -> PM [TStack Int]
>shiftAll tok [] = return []
>shiftAll (j,tok) stks
> = do	
>	let end = j + 1 
>	fid   <- addNode (FNode j end tok [])
>	stks' <- sequence [ do { nid <- getID ; return (push fid st nid end stk) }
>	                  | (stk,IBOX(st)) <- stks ]
>	return $ merge stks'

>pack 
> :: Int -> [TStack Int] -> (Branch, TStack Int, GSymbol) -> PM [TStack Int]
>-- {-## __A 2 __S LU(LLL)m __P $wpack 2 ##-};

>pack e_i stks (fids,stk,m)
> = do
>	nid <- getID
>	let st = goto (top stk) m
>	case fnd (\s -> UEQ(top s,st) && popF s == stk) stks of
>	 Nothing     -> do let s_i = endpoint stk
>			   fid <- addNode (FNode s_i e_i m [fids])
>			   return $ insertStack (push fid st nid e_i stk) stks
>	 Just (s,ss) -> do let oid = head (vals s)
>			   FNode s_i _ _ ch <- chgS (decElem oid)
>			   fid <- addNode $ FNode s_i e_i m (fids:ch)
>		    	   return $ insertStack (push fid st nid e_i stk) ss

>addNode :: ForestNode -> PM ForestId
>addNode = chgS . addElem


%----------------------------------------------------------------------------
Monad
TODO (pcc): combine the s/i, or use the modern libraries - might be faster?

>data ST s i a = MkST (s -> i -> (a,s,i))

>instance Functor (ST s i) where
> fmap f (MkST sf) 
>  = MkST $ \s i -> case sf s i of (a,s',i') -> (f a,s',i')

>instance Monad (ST s i) where
> return a = MkST $ \s i -> (a,s,i)
> MkST sf >>= k
>  = MkST $ \s i ->
>	case sf s i of
>	 (a,s',i') -> let (MkST sf') = k a in  sf' s' i' 

>runST :: s -> i -> ST s i a -> (s,a)
>runST s i (MkST sf) = case sf s i of
>			   (a,s,_) -> (s,a)

>chgS :: (s -> (a,s)) -> ST s i a
>chgS sf = MkST $ \s i -> let (a,s') = sf s in (a,s',i)
  
>useS :: (s -> b) -> ST s i b
>useS fn = MkST $ \s i -> (fn s,s,i)

>getID :: ST s [Int] Int
>getID = MkST $ \s (i:is) -> (i,s,is)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Structure and operations for BagMap data type.
  (c) University of Durham, Ben Medlock - 2001


<> module BagMap ( BagMap
<>		, initSM
<>		, addElem
<>		, decElem 
<>		, indices
<>		, assocs
<>		, fnd )
<> where


%-----------------------------------------------------------------------------
A BagMap is an injective functional relation.

Thus, the following properties hold:
- each element in the BagMap must occur only once 
  (as it would do in a normal Set) 
- each mapping must be unique 
  (no two elements can be mapped to by the same value)
 
These two properties must be upheld under all operations.

This implies the necessity to be able to apply an equality function
to both indices and elements.

See "A Tool for GLR Parsing in Haskell" (Ben Medlock, 2002) for more info.


%-----------------------------------------------------------------------------

>data BagMap a = SM ![Int]	                -- list of unused indices
>		     !(FiniteMap a Element)    	-- the relation

>data Element 
> = EL !Int    -- key assigned
>      !Int    -- reference count
>   deriving Show

%-----------------------------------------------------------------------------
Show instance

>instance Show a => Show (BagMap a) where
> show (SM _ rel)
>  = show [ (i,v) | (v, EL i _) <- fmToList rel ]


%-----------------------------------------------------------------------------
Main operations

>initSM :: BagMap a
>initSM = SM [0..] emptyFM

>{-# SPECIALIZE addElem :: ForestNode -> Forest -> (Int, Forest) #-}
>addElem :: Ord a => a -> BagMap a -> (Int,BagMap a)
>addElem e sm@(SM is@(i:it) rel) 
> = case lookupFM rel e of
>	Nothing       -> (i, SM it $ addToFM rel e (EL i 1))
>	Just (EL k j) -> (k, SM is $ addToFM rel e (EL k (j + 1)))
 
---
deletion of an item with an index
 - if the item has ref count 1, delete it entirely from the map
 - otherwise, decrement the count and return the map entry

>decElem :: Ord a => Int -> BagMap a -> (a,BagMap a)
>decElem i (SM is rel)
> = case {-# SCC "decElem_find" #-}
>        foldFM (\v (EL k r) t -> if k == i then (v,r) else t) not_found rel of
>     (v,0) -> error $ "Zero ref count in map at: " ++ show i
>     (v,1) -> v <> SM (i:is) (delFromFM rel v)
>     (v,n) -> v <> SM is     (addToFM rel v (EL i (n-1)))
>   where
>	not_found = error $ "Not found in map:" ++ show i

>assocs :: Show a => BagMap a -> [(Int,a)]
>assocs (SM _ rel) = [ (i,v) | (v, EL i _) <- fmToList rel ]

<> raw_map :: BagMap a -> [(Int,a)]
<> raw_map (SM _ rel) = fmToList rel 


%-----------------------------------------------------------------------------
Auxiliary functions 

<fnd> is exported as a useful operation for removing a single matching element
  from a list and returning the remaining elements. It does not really
  belong in this module though!
TODO: put somewhere

>fnd :: (a -> Bool) -> [a] -> Maybe (a,[a])
>fnd _ [] = Nothing
>fnd p (x:xs)  | p x       = Just (x,xs)
>		| otherwise = case fnd p xs of
>				Just (x',xs) -> Just (x',x:xs)
>				_	     -> Nothing

>(<>) x y = (x,y)  -- syntactic sugar



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

<> module TStack ( TStack
<>		, initTS
<>		, push
<>		, pop
<>		, popF
<>		, top
<>		, vals
<>		, height
<>		, merge )
<> where


>data TStack a 
> = TS { top      :: FAST_INT		-- state
>      , height   :: !Int	 	-- height
>      , ts_id    :: FAST_INT		-- ID
>      , endpoint :: !Int		-- end position of items in this stack
>      , ts_tail  :: ![(a,TStack a)]	-- [(element on arc , child)] 
>      }

>instance Show a => Show (TStack a) where
> show ts = "St" ++ show (IBOX(top ts)) ++ show (ts_tail ts)

---
id uniquely identifies a stack

>instance Show a => Eq (TStack a) where
> x == y = UEQ(ts_id x, ts_id y)
 
---
for ordering stack lists by height, ordering the tallest one first

>insertStack :: TStack a -> [TStack a] -> [TStack a]
>insertStack = insertBy tallestFirst

>tallestFirst :: TStack a -> TStack a -> Ordering
>tallestFirst x y = compare (height y) (height x)		-- note the reverse!

---

>initTS :: Int -> TStack a
>initTS IBOX(id) = TS ILIT(0) 1 id 0 []

>push :: a -> FAST_INT -> Int -> Int -> TStack a -> TStack a
>push x st IBOX(id) end stk = TS st (height stk + 1) id end [(x,stk)] 

>pop :: Int -> TStack a -> [([a],TStack a)] 
>pop 0 ts = [([],ts)]
>pop n ts = [ (xs ++ [x] , stk')
>	    | (x,stk) <- ts_tail ts
>	    , let rec = pop (n-1) stk
>	    , (xs,stk') <- rec ]

>popF :: TStack a -> TStack a 
>popF ts = case ts_tail ts of (_,c):_ -> c

>vals :: TStack a -> [a]
>vals ts = fst $ unzip $ ts_tail ts

>merge :: Show a => [TStack a] -> [TStack a]
>merge stks
> = [ TS st h id end ch
>   | IBOX(st) <- nub (map (\s -> IBOX(top s)) stks)
>   , let ch  = concat  [ x | TS st2 _ _ _ x <- stks, UEQ(st,st2) ]
>	  h   = maximum [ x | TS st2 x _ _ _ <- stks, UEQ(st,st2) ] 
>	  (IBOX(id),end) = head [ (IBOX(i),e) | TS st2 _ i e _ <- stks, UEQ(st,st2) ]
>   ]	-- poss merge these, if multi pass is a cost (see profile!)

