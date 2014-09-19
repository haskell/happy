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

> getNext :: Name -> (a -> (b, [Name], c, d)) -> (Name -> [a])
>         -> [(Name, IntSet)] -> [(Name, NameSet)]
> getNext fst_term prodNo prodsOfName env =
>               [ (nm, next nm) | (nm,_) <- env ]
>    where
>       fn t | t == errorTok || t >= fst_term = Set.singleton t
>       fn x = maybe (error "attempted FIRST(e) :-(") id (lookup x env)

>       next :: Name -> NameSet
>       next t | t >= fst_term = Set.singleton t
>       next n = Set.unions
>                       [ joinSymSets fn (snd4 (prodNo rl)) |
>                               rl <- prodsOfName n ]

My little hack

> snd4 :: (a, b, c, d) -> b
> snd4 (_,b,_,_) = b
