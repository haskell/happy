-----------------------------------------------------------------------------
Implementation of FIRST

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module First ( mkFirst ) where

> import GenUtils
> import NameSet ( NameSet )
> import qualified NameSet as Set
> import Grammar
> import Data.IntSet (IntSet)

\subsection{Utilities}

> joinSymSets :: (a -> NameSet) -> [a] -> NameSet
> joinSymSets f = foldr 
>       (\ h b -> let
>                   h' = f h
>                 in
>                    if incEmpty h'
>                    then Set.filter (not. isEmpty) h' `Set.union` b
>                    else h')
>        (Set.singleton epsilonTok)

Does the Set include the $\epsilon$ symbol ?

> incEmpty :: NameSet -> Bool
> incEmpty set = any isEmpty (Set.toAscList set)

\subsection{Implementation of FIRST}

> mkFirst :: Grammar -> [Name] -> NameSet
> mkFirst (Grammar { first_term = fst_term
>		   , lookupProdNo = prodNo
>		   , lookupProdsOfName = prodsOfName
>		   , non_terminals = nts
>		   })
>       = joinSymSets (\ h -> case lookup h env of
>                               Nothing -> Set.singleton h
>                               Just ix -> ix)
>   where
>       env = mkClosure (==) (getNext fst_term prodNo prodsOfName)
>               [ (name,Set.empty) | name <- nts ]

> getNext :: Name -> (a -> (b, [Name], c, d)) -> (Name -> [a])
>         -> [(Name, IntSet)] -> [(Name, NameSet)]
> getNext fst_term prodNo prodsOfName env = 
>		[ (nm, next nm) | (nm,_) <- env ]
>    where 
>    	fn t | t == errorTok || t >= fst_term = Set.singleton t
>    	fn x = case lookup x env of
>           	        Just t -> t
>                       Nothing -> error "attempted FIRST(e) :-("

> 	next :: Name -> NameSet
> 	next t | t >= fst_term = Set.singleton t
> 	next n = 
>       	foldb Set.union 
>               	[ joinSymSets fn (snd4 (prodNo rl)) | 
>				rl <- prodsOfName n ]

My little hack

> snd4 :: (a, b, c, d) -> b
> snd4 (_,b,_,_) = b
