%-----------------------------------------------------------------------------
First.lhs
(c) 1993 Andy Gill, Simon Marlow
%-----------------------------------------------------------------------------

> module First
> 	( mkFirst,
>	
>	  GrammarInfo, Name, Set) where

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
>        (singletonSet Epsilon)

Does the Set include the $\epsilon$ symbol ?

> incEmpty :: Set Name -> Bool
> incEmpty set = any isEmpty (setToList set)

\subsection{Implementation of FIRST}

> mkFirst :: GrammarInfo -> [Name] -> Set Name
> mkFirst prod = 
>       joinSymSets (\ h -> case assocMaybe env h of
>                               Nothing -> singletonSet h
>                               Just ix -> ix)
>   where
>       env = mkClosure (==) (getNext prod) 
>               [ (name,emptySet) | name <- getNonTerminals prod ]

> getNext :: GrammarInfo -> [(Name,Set Name)] -> [(Name,Set Name)]
> getNext prod env = [ (nm, next fn nm) | (nm,_) <- env ]
>    where 
>    	fn t@(Terminal _) = singletonSet t
>    	fn x = case assocMaybe env x of
>           	        Just t -> t
>                       Nothing -> error "attempted FIRST(e) :-("

> 	next :: (Name -> Set Name) -> Name -> Set Name
> 	next f name@(NonTerminal _) = 
>       	unionManySets 
>               	[ joinSymSets f lhs | (rl,lhs,_) <- getProds prod, 
>					      rl == name ]
> 	next _   name  = singletonSet name  
