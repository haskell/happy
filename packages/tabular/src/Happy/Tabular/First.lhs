-----------------------------------------------------------------------------
Implementation of FIRST

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Tabular.First ( mkFirst ) where

> import Happy.Core.GenUtils
> import Happy.Core.NameSet ( NameSet )
> import qualified Happy.Core.NameSet as Set
> import Happy.Core.Grammar
> import Data.IntSet (IntSet)

\subsection{Utilities}

> joinSymSets :: (a -> NameSet) -> [a] -> NameSet
> joinSymSets f = foldr go (Set.singleton epsilonTok) . map f
>    where
>       go h b
>           | Set.member epsilonTok h = Set.delete epsilonTok h `Set.union` b
>           | otherwise = h

\subsection{Implementation of FIRST}

> mkFirst :: Grammar -> [Name] -> NameSet
> mkFirst (Grammar { first_term = fst_term
>                  , lookupProdNo = prodNo
>                  , lookupProdsOfName = prodsOfName
>                  , non_terminals = nts
>                  })
>       = joinSymSets (\ h -> maybe (Set.singleton h) id (lookup h env) )
>   where
>       env = mkClosure (==) (getNext fst_term prodNo prodsOfName)
>               [ (name,Set.empty) | name <- nts ]

> getNext :: Name -> (a -> Production) -> (Name -> [a])
>         -> [(Name, IntSet)] -> [(Name, NameSet)]
> getNext fst_term prodNo prodsOfName env =
>               [ (nm, next nm) | (nm,_) <- env ]
>    where
>       fn t | t == errorTok || t >= fst_term = Set.singleton t
>       fn x = maybe (error "attempted FIRST(e) :-(") id (lookup x env)

>       next :: Name -> NameSet
>       next t | t >= fst_term = Set.singleton t
>       next n = Set.unions
>                       [ joinSymSets fn lhs
>                       | rl <- prodsOfName n
>                       , let Production _ lhs _ _ = prodNo rl ]

