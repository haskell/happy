> module Happy.Tabular (
>     Tables(..),
>     genTables,
>     SelectReductions,
>     select_all_reductions,
>     select_first_reduction
>   ) where

> import Happy.Grammar
> import Happy.Tabular.First
> import Happy.Tabular.LALR
> import Happy.Tabular.NameSet (NameSet)

> import Data.Array( Array, assocs, elems, (!) )
> import Data.List ( nub )

> data Tables =
>   Tables {
>     lr0items         :: [ItemSetWithGotos],
>     la_spont         :: [(Int, Lr0Item, NameSet)],
>     la_prop          :: Array Int [(Lr0Item, Int, Lr0Item)],
>     lookaheads       :: Array Int [(Lr0Item, NameSet)],
>     lr1items         :: [ ([Lr1Item], [(Name,Int)]) ],
>     gotoTable        :: GotoTable,
>     actionTable      :: ActionTable,
>     conflicts        :: (Array Int (Int,Int), (Int,Int)),
>     redundancies     :: ([Int], [String])
>   }

> genTables ::
>     SelectReductions ->     -- for computing used/unused
>     Grammar e ->
>     Tables
> genTables select_reductions g =
>       let first       = {-# SCC "First" #-} (mkFirst g)
>           closures    = {-# SCC "Closures" #-} (precalcClosure0 g)
>           lr0items    = {-# SCC "LR0_Sets" #-} (genLR0items g closures)
>           (la_spont, la_prop)
>                       = {-# SCC "Prop" #-} (propLookaheads g lr0items first)
>           lookaheads  = {-# SCC "Calc" #-} (calcLookaheads (length lr0items) la_spont la_prop)
>           lr1items    = {-# SCC "Merge" #-} (mergeLookaheadInfo lookaheads lr0items)
>           gotoTable   = {-# SCC "Goto" #-} (genGotoTable g lr0items)
>           actionTable = {-# SCC "Action" #-} (genActionTable g first lr1items)
>           conflicts   = {-# SCC "Conflict" #-} (countConflicts actionTable)
>           redundancies = find_redundancies select_reductions g actionTable
>       in Tables { lr0items, la_spont, la_prop, lookaheads, lr1items,
>                   gotoTable, actionTable, conflicts, redundancies }

-----------------------------------------------------------------------------
Find unused rules and tokens

> find_redundancies
>        :: SelectReductions -> Grammar e -> ActionTable -> ([Int], [String])
> find_redundancies extract_reductions g action_table =
>       (unused_rules, map (env !) unused_terminals)
>    where
>       Grammar { terminals = terms,
>                 token_names = env,
>                 eof_term = eof,
>                 starts = starts',
>                 productions = productions'
>               } = g
>       actions          = concat (map assocs (elems action_table))
>       start_rules      = [ 0 .. (length starts' - 1) ]
>       used_rules       = start_rules ++
>                          nub [ r | (_,a) <- actions, r <- extract_reductions a ]
>       used_tokens      = errorTok : catchTok : eof :
>                              nub [ t | (t,a) <- actions, is_shift a ]
>       n_prods          = length productions'
>       unused_terminals = filter (`notElem` used_tokens) terms
>       unused_rules     = filter (`notElem` used_rules ) [0..n_prods-1]

> is_shift :: LRAction -> Bool
> is_shift (LR'Shift _ _)             = True
> is_shift (LR'Multiple _ LR'Shift{}) = True
> is_shift _                          = False

---
selects what counts as a reduction when calculating used/unused

> type SelectReductions = LRAction -> [Int]

> select_all_reductions :: SelectReductions
> select_all_reductions = go
>   where go (LR'Reduce r _)    = [r]
>         go (LR'Multiple as a) = concatMap go (a : as)
>         go _                  = []

> select_first_reduction :: SelectReductions
> select_first_reduction = go
>   where go (LR'Reduce r _)   = [r]
>         go (LR'Multiple _ a) = go a   -- eg R/R conflict
>         go _                 = []
