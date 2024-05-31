-----------------------------------------------------------------------------
Implementation of FIRST

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Tabular.First ( mkFirst, mkClosure ) where

> import Happy.Tabular.NameSet ( NameSet )
> import qualified Happy.Tabular.NameSet as Set
> import Happy.Grammar
> import Data.Maybe (fromMaybe)

\subsection{Utilities}

> joinSymSets :: (a -> NameSet) -> [a] -> NameSet
> joinSymSets f = foldr go (Set.singleton epsilonTok) . map f
>    where
>       go h b
>           | Set.member epsilonTok h = Set.delete epsilonTok h `Set.union` b
>           | otherwise = h

@mkClosure@ makes a closure, when given a comparison and iteration loop.
It's a fixed point computation, we keep applying the function over the
input until it does not change.
Be careful, because if the functional always makes the object different,
This will never terminate.

> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = until (\x -> eq x (f x)) f

\subsection{Implementation of FIRST}

> mkFirst :: Grammar e -> [Name] -> NameSet
> mkFirst (Grammar { first_term = fst_term
>                  , lookupProdNo = prodNo
>                  , lookupProdsOfName = prodsOfName
>                  , non_terminals = nts
>                  })
>       = joinSymSets (\h -> fromMaybe (Set.singleton h) (lookup h env))
>   where
>       env = mkClosure (==) (updateFirstSets fst_term prodNo prodsOfName) [(name,Set.empty) | name <- nts]

> updateFirstSets :: Name -> (a -> Production e) -> (Name -> [a]) -> [(Name, NameSet)]
>                 -> [(Name, NameSet)]
> updateFirstSets fst_term prodNo prodsOfName env = [ (nm, nextFstSet nm)
>                                                   | (nm,_) <- env ]
>    where
>       terminalP :: Name -> Bool
>       terminalP s = s >= fst_term

>       currFstSet :: Name -> NameSet
>       currFstSet s | s == errorTok || s == catchTok || terminalP s = Set.singleton s
>                    | otherwise = maybe (error "attempted FIRST(e) :-(")
>                                    id (lookup s env)

>       nextFstSet :: Name -> NameSet
>       nextFstSet s | terminalP s = Set.singleton s
>                    | otherwise   = Set.unions [ joinSymSets currFstSet rhs
>                                               | rl <- prodsOfName s
>                                               , let Production _ rhs _ _ = prodNo rl ]
