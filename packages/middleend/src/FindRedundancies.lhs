> module FindRedundancies where

> import Grammar
> import Data.Array( assocs, elems, (!) )
> import Data.List

Find unused rules and tokens

> find_redundancies
>        :: (LRAction -> [Int]) -> Grammar -> ActionTable -> ([Int], [String])
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
>       used_tokens      = errorTok : eof :
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

> any_reduction :: LRAction -> [Int]
> any_reduction (LR'Reduce r _)    = [r]
> any_reduction (LR'Multiple as a) = concatMap any_reduction (a : as)
> any_reduction _                  = []

> first_reduction :: LRAction -> [Int]
> first_reduction (LR'Reduce r _)   = [r]
> first_reduction (LR'Multiple _ a) = first_reduction a   -- eg R/R conflict
> first_reduction _                 = []