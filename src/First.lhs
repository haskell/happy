-----------------------------------------------------------------------------
$Id: First.lhs,v 1.3 1997/07/16 13:32:33 simonm Exp $

Implementation of FIRST

(c) 1993-1996 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module First (mkFirst ) where

> import GenUtils
> import Set
> import AbsSyn
> import Grammar

\subsection{Utilities}

> joinSymSets :: (a -> Set Name) -> [a] -> Set Name
> joinSymSets f = foldr 
>       (\ h b -> let
>                   h' = f h
>                 in
>                    if incEmpty h'
>                    then filterSet (not. isEmpty) h' `union` b
>                    else h')
>        (singletonSet epsilonTok)

Does the Set include the $\epsilon$ symbol ?

> incEmpty :: Set Name -> Bool
> incEmpty set = any isEmpty (setToList set)

\subsection{Implementation of FIRST}

> mkFirst :: GrammarInfo -> [Name] -> Set Name
> mkFirst prod = 
>       joinSymSets (\ h -> case lookup h env of
>                               Nothing -> singletonSet h
>                               Just ix -> ix)
>   where
>       env = mkClosure (==) (getNext first_term prodNo prodsOfName)
>               [ (name,emptySet) | name <- getNonTerminals prod ]
>	first_term 	= getFirstTerm prod
>	prodNo 		= lookupProdNo prod
>	prodsOfName 	= lookupProdsOfName prod

> getNext first_term prodNo prodsOfName env = 
>		[ (nm, next nm) | (nm,_) <- env ]
>    where 
>    	fn t | t == -1 || t >= first_term = singletonSet t
>    	fn x = case lookup x env of
>           	        Just t -> t
>                       Nothing -> error "attempted FIRST(e) :-("

> 	next :: Name -> Set Name
> 	next t | t >= first_term = singletonSet t
> 	next n = 
>       	unionManySets 
>               	[ joinSymSets fn (snd3 (prodNo rl)) | 
>				rl <- prodsOfName n ]
