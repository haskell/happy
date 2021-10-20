-----------------------------------------------------------------------------
Generation of LALR parsing tables.

(c) 1993-1996 Andy Gill, Simon Marlow
(c) 1997-2001 Simon Marlow
-----------------------------------------------------------------------------

> module LALR
>       (ActionTable, GotoTable,
>        genActionTable, genGotoTable, genLR0items, precalcClosure0,
>        propLookaheads, calcLookaheads, mergeLookaheadInfo, countConflicts,
>        Lr0Item(..), Lr1Item(..), ItemSetWithGotos, LRAction(..), Goto(..))
>       where

> import GenUtils
> import Data.Set ( Set )
> import qualified Data.Set as Set hiding ( Set )
> import qualified NameSet
> import NameSet ( NameSet )
> import Happy.Grammar

> import Control.Monad (guard)
> import Control.Monad.ST
> import Data.Array.ST
> import Data.Array as Array
> import Data.List (nub,foldl',groupBy,sortBy)
> import Data.Function (on)
> import Data.Maybe (listToMaybe, maybeToList)

> unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
> unionMap f = Set.foldr (Set.union . f) Set.empty

> unionNameMap :: (Name -> NameSet) -> NameSet -> NameSet
> unionNameMap f = NameSet.foldr (NameSet.union . f) NameSet.empty

-----------------------------------------------------------------------------

This means rule $a$, with dot at $b$ (all starting at 0)

> data Lr0Item = Lr0 {-#UNPACK#-}!Int {-#UNPACK#-}!Int          -- (rule, dot)
>       deriving (Eq,Ord,Show)

> data Lr1Item = Lr1 {-#UNPACK#-}!Int {-#UNPACK#-}!Int NameSet  -- (rule, dot, lookahead)
>       deriving (Show)

> type RuleList = [Lr0Item]

> type ItemSetWithGotos = (Set Lr0Item, [(Name,Int)])

> data LRAction = LR'Shift Int Priority -- state number and priority
>               | LR'Reduce Int Priority-- rule no and priority
>               | LR'Accept             -- :-)
>               | LR'Fail               -- :-(
>               | LR'MustFail           -- :-(
>               | LR'Multiple [LRAction] LRAction       -- conflict
>       deriving (Eq,Show)

> type ActionTable = Array Int{-state-} (Array Int{-terminal#-} LRAction)
> type GotoTable = Array Int{-state-} (Array Int{-nonterminal #-} Goto)
> data Goto = Goto Int | NoGoto
>       deriving (Eq, Show)

-----------------------------------------------------------------------------
Generating the closure of a set of LR(0) items

Precalculate the rule closure for each non-terminal in the grammar,
using a memo table so that no work is repeated.

> precalcClosure0 :: Grammar -> Name -> RuleList
> precalcClosure0 g =
>       \n -> maybe [] id (lookup n info')
>  where
>
>       info' :: [(Name, RuleList)]
>       info' = map (\(n,rules) -> (n,map (\rule -> Lr0 rule 0) (NameSet.toAscList rules))) info

>       info :: [(Name, NameSet)]
>       info = mkClosure (==) (\f -> map (follow f) f)
>                       (map (\nt -> (nt,NameSet.fromList (lookupProdsOfName g nt))) nts)

>       follow :: [(Name, NameSet)] -> (Name, NameSet) -> (Name, NameSet)
>       follow f (nt,rules) = (nt, unionNameMap (followNT f) rules `NameSet.union` rules)

>       followNT :: [(Name, NameSet)] -> Int -> NameSet
>       followNT f rule =
>               case findRule g rule 0 of
>                       Just nt | nt >= firstStartTok && nt < fst_term ->
>                               maybe (error "followNT") id (lookup nt f)
>                       _ -> NameSet.empty

>       nts = non_terminals g
>       fst_term = first_term g

> closure0 :: Grammar -> (Name -> RuleList) -> Set Lr0Item -> Set Lr0Item
> closure0 g closureOfNT set = Set.foldr addRules Set.empty set
>    where
>       fst_term = first_term g
>       addRules rule set' = Set.union (Set.fromList (rule : closureOfRule rule)) set'
>
>       closureOfRule (Lr0 rule dot) =
>           case findRule g rule dot of
>               (Just nt) | nt >= firstStartTok && nt < fst_term
>                  -> closureOfNT nt
>               _  -> []

-----------------------------------------------------------------------------
Generating the closure of a set of LR(1) items

> closure1 :: Grammar -> ([Name] -> NameSet) -> [Lr1Item] -> [Lr1Item]
> closure1 g first set
>       = fst (mkClosure (\(_,new) _ -> null new) addItems ([],set))
>       where
>       fst_term = first_term g

>       addItems :: ([Lr1Item],[Lr1Item]) -> ([Lr1Item],[Lr1Item])
>       addItems (old_items, new_items) = (new_old_items, new_new_items)
>         where
>               new_old_items = new_items `union_items` old_items
>               new_new_items = subtract_items
>                                  (foldr union_items [] (map fn new_items))
>                                       new_old_items

>               fn :: Lr1Item -> [Lr1Item]
>               fn (Lr1 rule dot as) = case drop dot lhs of
>                       (b:beta) | b >= firstStartTok && b < fst_term ->
>                           let terms = unionNameMap
>                                               (\a -> first (beta ++ [a])) as
>                           in
>                           [ (Lr1 rule' 0 terms) | rule' <- lookupProdsOfName g b ]
>                       _ -> []
>                   where Production _name lhs _ _ = lookupProdNo g rule

Subtract the first set of items from the second.

> subtract_items :: [Lr1Item] -> [Lr1Item] -> [Lr1Item]
> subtract_items items1 items2 = foldr (subtract_item items2) [] items1

These utilities over item sets are crucial to performance.

Stamp on overloading with judicious use of type signatures...

> subtract_item :: [Lr1Item] -> Lr1Item -> [Lr1Item] -> [Lr1Item]
> subtract_item [] i result = i : result
> subtract_item ((Lr1 rule dot as):items) i@(Lr1 rule' dot' as') result =
>       case compare rule' rule of
>               LT -> i : result
>               GT -> carry_on
>               EQ -> case compare dot' dot of
>                       LT -> i : result
>                       GT -> carry_on
>                       EQ -> case NameSet.difference as' as of
>                               bs | NameSet.null bs -> result
>                                  | otherwise -> (Lr1 rule dot bs) : result
>  where
>       carry_on = subtract_item items i result

Union two sets of items.

> union_items :: [Lr1Item] -> [Lr1Item] -> [Lr1Item]
> union_items is [] = is
> union_items [] is = is
> union_items (i@(Lr1 rule dot as):is) (i'@(Lr1 rule' dot' as'):is') =
>       case compare rule rule' of
>               LT -> drop_i
>               GT -> drop_i'
>               EQ -> case compare dot dot' of
>                       LT -> drop_i
>                       GT -> drop_i'
>                       EQ -> (Lr1 rule dot (as `NameSet.union` as')) : union_items is is'
>  where
>       drop_i  = i  : union_items is (i':is')
>       drop_i' = i' : union_items (i:is) is'

-----------------------------------------------------------------------------
goto(I,X) function

The input should be the closure of a set of kernel items I together with
a token X (terminal or non-terminal.  Output will be the set of kernel
items for the set of items goto(I,X)

> gotoClosure :: Grammar -> Set Lr0Item -> Name -> Set Lr0Item
> gotoClosure gram i x = unionMap fn i
>    where
>       fn (Lr0 rule_no dot) =
>          case findRule gram rule_no dot of
>               Just t | x == t -> Set.singleton (Lr0 rule_no (dot+1))
>               _ -> Set.empty

-----------------------------------------------------------------------------
Generating LR0 Item sets

The item sets are generated in much the same way as we find the
closure of a set of items: we use two sets, those which have already
generated more sets, and those which have just been generated.  We
keep iterating until the second set is empty.

The addItems function is complicated by the fact that we need to keep
information about which sets were generated by which others.

> genLR0items :: Grammar -> (Name -> RuleList) -> [ItemSetWithGotos]
> genLR0items g precalcClosures
>       = fst (mkClosure (\(_,new) _ -> null new)
>               addItems
>                 (([],startRules)))
>  where

>    n_starts = length (starts g)
>    startRules :: [Set Lr0Item]
>    startRules = [ Set.singleton (Lr0 rule 0) | rule <- [0..n_starts] ]

>    tokens = non_terminals g ++ terminals g

>    addItems :: ([ItemSetWithGotos], [Set Lr0Item])
>             -> ([ItemSetWithGotos], [Set Lr0Item])
>
>    addItems (oldSets,newSets) = (newOldSets, reverse newNewSets)
>     where
>
>       newOldSets = oldSets ++ (zip newSets intgotos)

>       itemSets = map fst oldSets ++ newSets

First thing to do is for each set in I in newSets, generate goto(I,X)
for each token (terminals and nonterminals) X.

>       gotos :: [[(Name,Set Lr0Item)]]
>       gotos = map (filter (not . Set.null . snd))
>           (map (\i -> let i' = closure0 g precalcClosures i in
>                       [ (x,gotoClosure g i' x) | x <- tokens ]) newSets)

Next, we assign each new set a number, which is the index of this set
in the list of sets comprising all the sets generated so far plus
those generated in this iteration.  We also filter out those sets that
are new, i.e. don't exist in the current list of sets, so that they
can be added.

We also have to make sure that there are no duplicate sets in the
*current* batch of goto(I,X) sets, as this could be disastrous.  I
think I've squished this one with the '++ reverse newSets' in
numberSets.

numberSets is built this way so we can use it quite neatly with a foldr.
Unfortunately, the code's a little opaque.

>       numberSets
>               :: [(Name,Set Lr0Item)]
>               -> (Int,
>                   [[(Name,Int)]],
>                   [Set Lr0Item])
>               -> (Int, [[(Name,Int)]], [Set Lr0Item])
>
>       numberSets [] (i,gotos',newSets') = (i,([]:gotos'),newSets')
>       numberSets ((x,gotoix):rest) (i,g':gotos',newSets')
>          = numberSets rest
>               (case indexInto 0 gotoix (itemSets ++ reverse newSets') of
>                       Just j  -> (i,  ((x,j):g'):gotos', newSets')
>                       Nothing -> (i+1,((x,i):g'):gotos', gotoix:newSets'))
>       numberSets _ _ = error "genLR0items/numberSets: Unhandled case"

Finally, do some fiddling around to get this all in the form we want.

>       intgotos :: [[(Name,Int)]]
>       newNewSets  :: [Set Lr0Item]
>       (_, ([]:intgotos), newNewSets) =
>               foldr numberSets (length newOldSets, [[]], []) gotos

> indexInto :: Eq a => Int -> a -> [a] -> Maybe Int
> indexInto _ _ []                 = Nothing
> indexInto i x (y:ys) | x == y    = Just i
>                      | otherwise = let j = i + 1 in j `seq` indexInto j x ys

-----------------------------------------------------------------------------
Computing propagation of lookaheads

ToDo: generate this info into an array to be used in the subsequent
calcLookaheads pass.

> propLookaheads
>       :: Grammar
>       -> [(Set Lr0Item,[(Name,Int)])]         -- LR(0) kernel sets
>       -> ([Name] -> NameSet)                  -- First function
>       -> (
>               [(Int, Lr0Item, NameSet)],      -- spontaneous lookaheads
>               Array Int [(Lr0Item, Int, Lr0Item)]     -- propagated lookaheads
>          )

> propLookaheads gram sets first = (concat s, array (0,length sets - 1)
>                       [ (a,b) | (a,b) <- p ])
>   where

>     (s,p) = unzip (zipWith propLASet sets [0..])

>     propLASet :: (Set Lr0Item, [(Name, Int)]) -> Int -> ([(Int, Lr0Item, NameSet)],(Int,[(Lr0Item, Int, Lr0Item)]))
>     propLASet (set,goto) i = (start_spont ++ concat s', (i, concat p'))
>       where

>         (s',p') = unzip (map propLAItem (Set.toAscList set))

>         -- spontaneous EOF lookaheads for each start state & rule...
>         start_info :: [(String, Name, Name, Bool)]
>         start_info = starts gram

>         start_spont :: [(Int, Lr0Item ,NameSet)]
>         start_spont   = [ (start, (Lr0 start 0),
>                            NameSet.singleton (startLookahead gram partial))
>                         | (start, (_,_,_,partial)) <-
>                               zip [0..] start_info]

>         propLAItem :: Lr0Item -> ([(Int, Lr0Item, NameSet)], [(Lr0Item, Int, Lr0Item)])
>         propLAItem item@(Lr0 rule dot) = (spontaneous, propagated)
>           where
>               lookupGoto msg x = maybe (error msg) id (lookup x goto)

>               j = closure1 gram first [Lr1 rule dot (NameSet.singleton dummyTok)]

>               spontaneous :: [(Int, Lr0Item, NameSet)]
>               spontaneous = do
>                   (Lr1 rule' dot' ts) <- j
>                   let ts' = NameSet.delete dummyTok ts
>                   guard (not $ NameSet.null ts')
>                   maybeToList $ do r <- findRule gram rule' dot'
>                                    return ( lookupGoto "spontaneous" r
>                                           , Lr0 rule' (dot' + 1)
>                                           , ts' )

>               propagated :: [(Lr0Item, Int, Lr0Item)]
>               propagated = do
>                   (Lr1 rule' dot' ts) <- j
>                   guard $ NameSet.member dummyTok ts
>                   maybeToList $ do r <- findRule gram rule' dot'
>                                    return ( item
>                                           , lookupGoto "propagated" r
>                                           , Lr0 rule' (dot' + 1) )

The lookahead for a start rule depends on whether it was declared
with %name or %partial: a %name parser is assumed to parse the whole
input, ending with EOF, whereas a %partial parser may parse only a
part of the input: it accepts when the error token is found.

> startLookahead :: Grammar -> Bool -> Name
> startLookahead gram partial = if partial then errorTok else eof_term gram

-----------------------------------------------------------------------------
Calculate lookaheads

Special version using a mutable array:

> calcLookaheads
>       :: Int                                  -- number of states
>       -> [(Int, Lr0Item, NameSet)]            -- spontaneous lookaheads
>       -> Array Int [(Lr0Item, Int, Lr0Item)]  -- propagated lookaheads
>       -> Array Int [(Lr0Item, NameSet)]

> calcLookaheads n_states spont prop
>       = runST $ do
>           arr <- newArray (0,n_states) []
>           propagate arr (fold_lookahead spont)
>           freeze arr

>   where
>       propagate :: STArray s Int [(Lr0Item, NameSet)]
>                        -> [(Int, Lr0Item, NameSet)] -> ST s ()
>       propagate _   []  = return ()
>       propagate arr new = do
>               let
>                  items = [ (i,item'',s) | (j,item,s) <- new,
>                                           (item',i,item'') <- prop ! j,
>                                           item == item' ]
>               new_new <- get_new arr items []
>               add_lookaheads arr new
>               propagate arr new_new

This function is needed to merge all the (set_no,item,name) triples
into (set_no, item, set name) triples.  It can be removed when we get
the spontaneous lookaheads in the right form to begin with (ToDo).

> add_lookaheads :: STArray s Int [(Lr0Item, NameSet)]
>                -> [(Int, Lr0Item, NameSet)]
>                -> ST s ()
> add_lookaheads arr = mapM_ $ \(i,item,s)
>                    -> do las <- readArray arr i
>                          writeArray arr i (add_lookahead item s las)

> get_new :: STArray s Int [(Lr0Item, NameSet)]
>         -> [(Int, Lr0Item, NameSet)]
>         -> [(Int, Lr0Item, NameSet)]
>         -> ST s [(Int, Lr0Item, NameSet)]
> get_new _   []                   new = return new
> get_new arr (l@(i,_item,_s):las) new = do
>       state_las <- readArray arr i
>       get_new arr las (get_new' l state_las new)

> add_lookahead :: Lr0Item -> NameSet -> [(Lr0Item,NameSet)] ->
>                       [(Lr0Item,NameSet)]
> add_lookahead item s [] = [(item,s)]
> add_lookahead item s (m@(item',s') : las)
>       | item == item' = (item, s `NameSet.union` s') : las
>       | otherwise     = m : add_lookahead item s las

> get_new' :: (Int,Lr0Item,NameSet) -> [(Lr0Item,NameSet)] ->
>                [(Int,Lr0Item,NameSet)] -> [(Int,Lr0Item,NameSet)]
> get_new' l [] new = l : new
> get_new' l@(i,item,s) ((item',s') : las) new
>       | item == item' =
>               let s'' = s NameSet.\\ s' in
>               if NameSet.null s'' then new else (i,item,s'') : new
>       | otherwise =
>               get_new' l las new

> fold_lookahead :: [(Int,Lr0Item,NameSet)] -> [(Int,Lr0Item,NameSet)]
> fold_lookahead =
>     map (\cs@(((a,b),_):_) -> (a,b,NameSet.unions $ map snd cs)) .
>     groupBy ((==) `on` fst) .
>     sortBy (compare `on` fst) .
>     map (\(a,b,c) -> ((a,b),c))

-----------------------------------------------------------------------------
Merge lookaheads

Stick the lookahead info back into the state table.

> mergeLookaheadInfo
>       :: Array Int [(Lr0Item, NameSet)]       -- lookahead info
>       -> [(Set Lr0Item, [(Name,Int)])]        -- state table
>       -> [ ([Lr1Item], [(Name,Int)]) ]

> mergeLookaheadInfo lookaheads sets
>       = zipWith mergeIntoSet sets [0..]
>       where

>         mergeIntoSet :: (Set Lr0Item, [(Name, Int)]) -> Int -> ([Lr1Item], [(Name, Int)])
>         mergeIntoSet (items, goto) i
>               = (map mergeIntoItem (Set.toAscList items), goto)
>               where

>                 mergeIntoItem :: Lr0Item -> Lr1Item
>                 mergeIntoItem item@(Lr0 rule dot) = Lr1 rule dot la
>                    where la = case [ s | (item',s) <- lookaheads ! i,
>                                           item == item' ] of
>                                       [] -> NameSet.empty
>                                       [x] -> x
>                                       _ -> error "mergIntoItem"

-----------------------------------------------------------------------------
Generate the goto table

This is pretty straightforward, given all the information we stored
while generating the LR0 sets of items.

Generating the goto table doesn't need lookahead info.

> genGotoTable :: Grammar -> [(Set Lr0Item,[(Name,Int)])] -> GotoTable
> genGotoTable g sets = gotoTable
>   where
>       Grammar{ first_nonterm = fst_nonterm,
>                first_term    = fst_term,
>                non_terminals = non_terms } = g
>
>       -- goto array doesn't include %start symbols
>       gotoTable  = listArray (0,length sets-1)
>         [
>           (array (fst_nonterm, fst_term-1) [
>               (n, maybe NoGoto Goto (lookup n goto))
>                             | n <- non_terms,
>                               n >= fst_nonterm, n < fst_term ])
>                 | (_set,goto) <- sets  ]

-----------------------------------------------------------------------------
Generate the action table

> genActionTable :: Grammar -> ([Name] -> NameSet) ->
>                [([Lr1Item],[(Name,Int)])] -> ActionTable
> genActionTable g first sets = actionTable
>   where
>       Grammar { first_term = fst_term,
>                 terminals = terms,
>                 starts = starts',
>                 priorities = prios } = g

>       n_starts = length starts'
>       isStartRule rule = rule < n_starts -- a bit hacky, but it'll do for now

>       term_lim = (head terms,last terms)
>       actionTable = array (0,length sets-1)
>             [ (set_no, accumArray res
>                                LR'Fail term_lim
>                               (possActions goto set))
>                   | ((set,goto),set_no) <- zip sets [0..] ]

>       possAction goto _set (Lr1 rule pos la) =
>          case findRule g rule pos of
>               Just t | t >= fst_term || t == errorTok ->
>                       let f j = (t,LR'Shift j p)
>                           p = maybe No id (lookup t prios)
>                       in map f $ maybeToList (lookup t goto)
>               Nothing
>                  | isStartRule rule
>                  -> let (_,_,_,partial) = starts' !! rule in
>                     [ (startLookahead g partial, LR'Accept{-'-}) ]
>                  | otherwise
>                  -> let Production _ _ _ p = lookupProdNo g rule in
>                     NameSet.toAscList la `zip` repeat (LR'Reduce rule p)
>               _ -> []

>       possActions goto coll = do item <- closure1 g first coll
>                                  possAction goto coll item

These comments are now out of date! /JS

Here's how we resolve conflicts, leaving a complete record of the
conflicting actions in an LR'Multiple structure for later output in
the info file.

Shift/reduce conflicts are always resolved as shift actions, and
reduce/reduce conflicts are resolved as a reduce action using the rule
with the lowest number (i.e. the rule that comes first in the grammar
file.)

NOTES on LR'MustFail: this was introduced as part of the precedence
parsing changes.  The problem with LR'Fail is that it is a soft
failure: we sometimes substitute an LR'Fail for an LR'Reduce (eg. when
computing default actions), on the grounds that an LR'Fail in this
state will also be an LR'Fail in the goto state, so we'll fail
eventually.  This may not be true with precedence parsing, though.  If
there are two non-associative operators together, we must fail at this
point rather than reducing.  Hence the use of LR'MustFail.


NOTE: on (LR'Multiple as a) handling
      PCC [sep04] has changed this to have the following invariants:
        * the winning action appears only once, in the "a" slot
        * only reductions appear in the "as" list
        * there are no duplications
      This removes complications elsewhere, where LR'Multiples were
      building up tree structures...

>       res LR'Fail x = x
>       res x LR'Fail = x
>       res LR'MustFail _ = LR'MustFail
>       res _ LR'MustFail = LR'MustFail
>       res x x' | x == x' = x
>       res (LR'Accept) _ = LR'Accept
>       res _ (LR'Accept) = LR'Accept

>       res (LR'Multiple as x) (LR'Multiple bs x')
>        | x == x' = LR'Multiple (nub $ as ++ bs) x
>               -- merge dropped reductions for identical action

>        | otherwise
>              = case res x x' of
>                  LR'Multiple cs a
>                    | a == x    -> LR'Multiple (nub $ x' : as ++ bs ++ cs) x
>                    | a == x'   -> LR'Multiple (nub $ x  : as ++ bs ++ cs) x'
>                    | otherwise -> error "failed invariant in resolve"
>                               -- last means an unexpected change
>                  other -> other
>               -- merge dropped reductions for clashing actions, but only
>               -- if they were S/R or R/R

>       res a@(LR'Multiple _ _) b = res a (LR'Multiple [] b)
>       res a b@(LR'Multiple _ _) = res (LR'Multiple [] a) b
>         -- leave cases above to do the appropriate merging

>       res a@(LR'Shift {}) b@(LR'Reduce {}) = res b a
>       res a@(LR'Reduce _ p) b@(LR'Shift _ p')
>               = case (p,p') of
>                      (PrioLowest,PrioLowest) -> LR'MustFail
>                      (_,PrioLowest) -> a
>                      (PrioLowest,_) -> b
>                      (No,_) -> LR'Multiple [a] b      -- shift wins
>                      (_,No) -> LR'Multiple [a] b      -- shift wins
>                      (Prio c i, Prio _ j)
>                               | i < j     -> b
>                               | i > j     -> a
>                               | otherwise ->
>                                  case c of
>                                     LeftAssoc  -> a
>                                     RightAssoc -> b
>                                     None       -> LR'MustFail
>       res a@(LR'Reduce r p) b@(LR'Reduce r' p')
>               = case (p,p') of
>                      (PrioLowest,PrioLowest) ->
>                        LR'Multiple [a] b      -- give to earlier rule?
>                      (_,PrioLowest) -> a
>                      (PrioLowest,_) -> b
>                      (No,_) -> LR'Multiple [a] b      -- give to earlier rule?
>                      (_,No) -> LR'Multiple [a] b
>                      (Prio _ i, Prio _ j)
>                               | i < j     -> b
>                               | j > i     -> a
>                               | r < r'    -> LR'Multiple [b] a
>                               | otherwise -> LR'Multiple [a] b
>       res _ _ = error "confict in resolve"

-----------------------------------------------------------------------------
Count the conflicts

> countConflicts :: ActionTable -> (Array Int (Int,Int), (Int,Int))
> countConflicts action
>   = (conflictArray, foldl' (\(a,b) (c,d) -> let ac = a + c; bd = b + d in ac `seq` bd `seq` (ac,bd)) (0,0) conflictList)
>
>   where
>
>       conflictArray = listArray (Array.bounds action) conflictList
>       conflictList  = map countConflictsState (assocs action)
>
>       countConflictsState (_state, actions)
>         = foldr countMultiples (0,0) (elems actions)
>         where
>           countMultiples (LR'Multiple (_:_) (LR'Shift{})) (sr,rr)
>               = (sr + 1, rr)
>           countMultiples (LR'Multiple (_:_) (LR'Reduce{})) (sr,rr)
>               = (sr, rr + 1)
>           countMultiples (LR'Multiple _ _) _
>               = error "bad conflict representation"
>           countMultiples _ c = c

-----------------------------------------------------------------------------

> findRule :: Grammar -> Int -> Int -> Maybe Name
> findRule g rule dot = listToMaybe (drop dot lhs)
>     where Production _ lhs _ _ = lookupProdNo g rule
