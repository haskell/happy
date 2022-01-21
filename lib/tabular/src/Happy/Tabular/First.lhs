-----------------------------------------------------------------------------
Implementation of FIRST

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Tabular.First ( mkFirst, mkClosure ) where

> import Happy.Tabular.NameSet ( NameSet )
> import qualified Happy.Tabular.NameSet as Set
> import Happy.Grammar

\subsection{Utilities}

> joinSymSets :: (a -> NameSet) -> [a] -> NameSet
> joinSymSets f = foldr go (Set.singleton epsilonTok) . map f
>    where
>       go h b
>           | Set.member epsilonTok h = Set.delete epsilonTok h `Set.union` b
>           | otherwise = h

@mkClosure@ makes a closure, when given a comparison and iteration loop.
Be careful, because if the functional always makes the object different,
This will never terminate.

> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = match . iterate f
>   where
>       match (a:b:_) | a `eq` b = a
>       match (_:c)              = match c
>       match [] = error "Can't happen: match []"

\subsection{Implementation of FIRST}

> mkFirst :: Grammar e -> [Name] -> NameSet
> mkFirst (Grammar { first_term = fst_term
>                  , lookupProdNo = prodNo
>                  , lookupProdsOfName = prodsOfName
>                  , non_terminals = nts
>                  })
>       = joinSymSets (\ h -> maybe (Set.singleton h) id (lookup h env) )
>   where
>       env = mkClosure (==) (getNext fst_term prodNo prodsOfName)
>               [ (name,Set.empty) | name <- nts ]

> getNext :: Name -> (a -> Production e) -> (Name -> [a])
>         -> [(Name, NameSet)] -> [(Name, NameSet)]
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

