>{-# LINE 1 "GLR_Lib.lhs" #-}

{-
   GLR_Lib.lhs
   $Id: GLR_Lib.lhs,v 1.4 2004/09/08 10:42:28 paulcc Exp $
-}

> {-
> Parser driver for the GLR parser.
> 
> (c) University of Durham, Ben Medlock 2001
>         -- initial code, for structure parsing
> (c) University of Durham, Paul Callaghan 2004
>         -- extension to semantic rules
>         -- shifting to chart data structure
>         -- supporting hidden left recursion
>         -- many optimisations
> -}

{- supplied by Happy 
<> module XYZ ( 
<>              lexer	-- conditional
-}

>	-- probable, but might want to parametrise
>           , doParse
>           , TreeDecode(..), decode	-- only for tree decode
>           , LabelDecode(..)		-- only for label decode

>	-- standard exports
>           , Tokens
>           , GLRResult(..)
>           , NodeMap
>           , RootNode
>           , ForestId
>           , GSymbol(..)
>           , Branch(..)
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

>fakeimport DATA

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

A forest is a map of `spans' to branches, where a span is a start position,
and end position, and a grammatical category for that interval. Branches
are lists of conjunctions of symbols which can be matched in that span.
Note that tokens are stored as part of the spans.

>type Forest       = FiniteMap ForestId [Branch]

---
End result of parsing: 
 - successful parse with rooted forest
 - else syntax error or premature eof

>type NodeMap = [(ForestId, [Branch])]
>type RootNode = ForestId
>type Tokens = [[(Int, GSymbol)]]	-- list of ambiguous lexemes

>data GLRResult 
> = ParseOK     RootNode Forest    -- forest with root
> | ParseError  Tokens   Forest    -- partial forest with bad input
> | ParseEOF             Forest    -- partial forest (missing input)

%-----------------------
Forest to simplified output

>forestResult :: Int -> Forest -> GLRResult
>forestResult length f
> = case roots of
>	[]       -> ParseEOF f
>	[r]      -> ParseOK r f
>	rs@(_:_) -> error $ "multiple roots in forest, = " ++ show rs
>						++ unlines (map show ns_map)
>   where
>	ns_map = fmToList f 
>	roots = [ r | (r@(0,sz,sym),_) <- ns_map
>	            , sz == length
>	            , sym == top_symbol ]


%----------------------------------------------------------------------------

>glr_parse :: [[UserDefTok]] -> GLRResult
>glr_parse toks 
> = case runST emptyFM [0..] (tp toks) of
>    (f,Left ts)   -> ParseError ts f 
>						-- Error within sentence
>    (f,Right ss)  -> forestResult (length toks) f
>						-- Either good parse or EOF
>   where
>	tp tss = doActions [initTS 0] 
>	       $ zipWith (\i ts -> [(i, t) | t <- ts]) [0..] 
>              $ [ [ HappyTok {-j-} t | (j,t) <- zip [0..] ts ] | ts <- tss ]
>                ++ [[HappyEOF]]

---

>type PM a = ST Forest [Int] a
>type FStack = TStack ForestId


---
main function

>doActions :: [FStack] -> Tokens -> PM (Either Tokens [FStack])

>doActions ss [] 		-- no more tokens (this is ok)
> = return (Right ss)		-- return the stacks (may be empty)

>doActions stks (tok:toks)
> = do
>	stkss <- sequence [ do
>                             stks' <- reduceAll [] tok_form stks 
>                             shiftAll tok_form stks'
>                         | tok_form <- tok ]
>	case merge $ concat stkss of 		-- did this token kill stacks?
>	  [] -> case toks of
>		  []  -> return $ Right []	   -- ok if no more tokens
>		  _:_ -> return $ Left (tok:toks)  -- not ok if some input left
>	  ss -> doActions ss toks

>reduceAll 
> :: [GSymbol] -> (Int, GSymbol) -> [FStack] -> PM [(FStack, Int)]
>reduceAll _ tok [] = return []
>reduceAll cyclic_names itok@(i,tok) (stk:stks)
> = case action this_state tok of
>    Accept       -> reduceAll [] itok stks
>    Error        -> -- trace ("Clash @ " ++ show (itok, top stk) ++ "\n")
>                    -- DISCARDS current stack - can't continue 
>                    reduceAll [] itok stks
>    Shift st rs -> do { ss <- redAll rs ; return $ (stk,st):ss } 
>    Reduce rs   -> redAll rs
> where 
>  this_state = top stk
>  redAll rs 
>   = do 
>	let reds = [ (bf fids,stk',m) 
>	           | (m,n,bf) <- rs
>	           , not (n == 0 && m `elem` cyclic_names)  -- remove done ones
>	           , (fids,stk') <- pop n stk
>	           ]
>	           -- WARNING: incomplete if more than one Empty in a prod(!)
>	           -- WARNING: can avoid by splitting emps/non-emps
>	stks' <- foldM (pack i) stks reds	
>	  -- can reduce the build/unbuild here...
>	let new_cyclic = [ m | (m,0,_) <- rs
>	                     , UEQ(this_state, goto this_state m)
>	                     , m `notElem` cyclic_names ]
>	reduceAll (cyclic_names ++ new_cyclic) itok stks' 

>shiftAll :: (Int, GSymbol) -> [(FStack, Int)] -> PM [FStack]
>shiftAll tok [] = return []
>shiftAll (j,tok) stks
> = do	
>	let end = j + 1 
>	let key = (j,end,tok)
>	newNode key
>	stks' <- sequence [ do { nid <- getID ; return (push key st nid end stk) }
>	                  | (stk,IBOX(st)) <- stks ]
>	return $ merge stks'

>pack 
> :: Int -> [FStack] -> (Branch, FStack, GSymbol) -> PM [FStack]
>-- {-## __A 2 __S LU(LLL)m __P $wpack 2 ##-};

>pack e_i stks (fids,stk,m)
> = do
>	nid <- getID
>	let s_i = endpoint stk
>	let st = goto (top stk) m
>	if ULT(st, ILIT(0)) then return stks else
>	 case fnd (\s -> UEQ(top s,st) && popF s == stk) stks of
>	  Nothing     -- new stack in set
>	              -> do -- let s_i = endpoint stk???
>                           let key = (s_i,e_i,m)
>                           addBranch key fids 
>                           return $ insertStack (push key st nid e_i stk) stks

>         Just (s,ss) -- pack into an existing stack
>                     -> do let oid = head (vals s)
>                           --let key = (s_i,e_i,m)
>                           let key = oid
>                           addBranch key fids
>                           return $ insertStack (push key st nid e_i stk) ss


---
record an entry
 - expected: "i" will contain a token

>newNode :: ForestId -> PM ()
>newNode i
> = chgS $ \f -> ((), addToFM f i [])

---
add a new branch
 - due to packing, we check to see if a branch is already there
 - if so, we avoid memory use
 - TODO (pcc): try to measure if this is worth doing.

>addBranch :: ForestId -> Branch -> PM ()
>addBranch i b 
> = do
>	f <- useS id
>	case lookupFM f i of 
>	  Nothing               -> chgS $ \f -> ((), addToFM f i [b])
>	  Just bs | b `elem` bs -> return ()
>	          | otherwise   -> chgS $ \f -> ((), addToFM f i (b:bs))

---
only for use with nodes that exist

>getBranches ::  ForestId -> PM [Branch]
>getBranches i 
> = useS $ \s -> lookupWithDefaultFM s no_such_node i
>   where
>	no_such_node = error $ "No such node in Forest: " ++ show i



%----------------------------------------------------------------------------
Monad
TODO (pcc): combine the s/i, or use the modern libraries - might be faster?
            but some other things are much, much, much more expensive! 

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
<>		, merge )
<> where


>data TStack a 
> = TS { top      :: FAST_INT		-- state
>      , ts_id    :: FAST_INT		-- ID
>      , endpoint :: !Int		-- end position of items in this stack
>      , stoup    :: !(Maybe a)		-- temp holding place, for left rec.
>      , ts_tail  :: ![(a,TStack a)]	-- [(element on arc , child)] 
>      }

>instance Show a => Show (TStack a) where
> show ts = "St" ++ show (IBOX(top ts), stoup ts) ++ show (ts_tail ts)

---
id uniquely identifies a stack

>instance Eq (TStack a) where
>      s1 == s2 = UEQ(ts_id s1, ts_id s2)

<>instance Ord (TStack a) where
<>      s1 `compare` s2 = IBOX(ts_id s1) `compare` IBOX(ts_id s2)
 
---
Nothing special done for insertion, but check this against frequent merging
on problem cases.

>insertStack :: TStack a -> [TStack a] -> [TStack a]
>insertStack = (:)

---

>initTS :: Int -> TStack a
>initTS IBOX(id) = TS ILIT(0) id 0 Nothing []

>--push :: a -> FAST_INT -> Int -> Int -> TStack a -> TStack a
>push x@(s_i,e_i,m) st IBOX(id) end stk 
> = TS st id end stoup [(x,stk)] 
>   where
>       stoup | s_i == e_i && UEQ(st, goto st m) = Just x	
>             | otherwise                        = Nothing
>	-- only fill stoup for cyclic states that don't consume input

>pop :: Int -> TStack a -> [([a],TStack a)] 
>pop 0 ts = [([],ts)]
>pop 1 st@TS{stoup=Just x}
> = pop 1 st{stoup=Nothing} ++ [ ([x],st) ] 
>pop n ts = [ (xs ++ [x] , stk')
>	    | (x,stk) <- ts_tail ts
>	    , let rec = pop (n-1) stk
>	    , (xs,stk') <- rec ]

>popF :: TStack a -> TStack a 
>popF ts = case ts_tail ts of (_,c):_ -> c

>vals :: TStack a -> [a]
>vals ts = fst $ unzip $ ts_tail ts

>--merge :: Show a => [TStack a] -> [TStack a]
>merge stks
> = [ TS st id end ss ch
>   | IBOX(st) <- nub (map (\s -> IBOX(top s)) stks)
>   , let ch  = concat  [ x | TS st2 _ _ _ x <- stks, UEQ(st,st2) ]
>	  ss  = mkss    [ s | TS st2 _ _ s _ <- stks, UEQ(st,st2) ]
>	  (IBOX(id),end) = head [ (IBOX(i),e) | TS st2 i e _ _ <- stks, UEQ(st,st2) ]
>	  -- reuse of id is ok, since merge discards old stacks
>   ]	-- poss merge these, if multi pass is a cost (see profile!)
>   where
>        mkss s = case nub [ x | Just x <- s ] of
>                   []  -> Nothing
>                   [x] -> Just x
>                   xs  -> error $ unlines $ ("Stoup merge: " ++ show xs) : map show stks


