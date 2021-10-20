> module Happy.Tabular (
>     Tables(..),
>     genTables,
>     SelectReductions,
>     select_all_reductions,
>     select_first_reduction
>   ) where

> import Happy.Grammar
> import Happy.Tabular.FindRedundancies
> import Happy.Tabular.First
> import Happy.Tabular.LALR
> import Happy.Tabular.Tables
> import Happy.Tabular.NameSet (NameSet)

> import Data.Array( Array )

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
>     Grammar ->
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
