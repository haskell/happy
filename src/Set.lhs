-----------------------------------------------------------------------------
$Id: Set.lhs,v 1.8 1998/06/19 13:41:06 simonm Exp $

A set ADT.

This code is derived from the Set Module in the Glasgow Haskell
Compiler, which in turn was derived from the utilities section of
Simon PJ's second book.  It is therefore *not* covered under the GNU
License.
-----------------------------------------------------------------------------

> module Set (
>        Set,
>        mkSet, setToList, emptySet, singletonSet,
>        union, union_Int, unionManySets,
>        elementOf, isEmptySet,
>        sizeOfSet, mapSet, concatSet, concatMapSet, filterSet,
>	 subtractSet
>    ) where

> import GenUtils

> type Set a = [a] 

 instance (Show a) => Show (Set a) where 
    showsPrec _ (MkSet [])     = showString "{}"
    showsPrec _ (MkSet (x:xs)) = showChar '{' . shows x . showl xs
                      where showl []     = showChar '}'
                            showl (x:xs) = showChar ',' . shows x . showl xs

#ifdef GOFER

 instance Eq (Set a) where { (==) = primGenericEq } 
 instance Ord [a] => Ord (Set a) where 
       (MkSet a) <= (MkSet b) = a <= b

#endif

This is where we order the list and remove duplicates.

> emptySet :: Ord a => Set a
> emptySet =  []

> singletonSet :: Ord a => a -> Set a
> singletonSet x = [x]

> setToList :: Ord a => Set a -> [a]
> setToList xs = xs

> mkSet :: (Ord a) => [a] -> Set a
> mkSet xs = sort_and_nuke_dups (<) (==) xs

-- slightly tweaked for performance...

 sort_and_nuke_dups :: (a->a->Bool) -> (a->a->Bool) -> [a] -> [a]

> sort_and_nuke_dups _ _ []  = []
> sort_and_nuke_dups _ _ [x] = [x]
> sort_and_nuke_dups lt eq xs  
>	= foldb (merge_with_nuke lt eq) (splitList lt eq xs)

> splitList lt eq (a1:a2:a3:a4:a5:xs) = 
>                insert_with_nuke lt eq a1 
>               (insert_with_nuke lt eq a2 
>               (insert_with_nuke lt eq a3
>               (insert_with_nuke lt eq a4 [a5]))) : splitList lt eq xs
> splitList _ _ [] = []
> splitList lt eq (r:rs) = [foldr (insert_with_nuke lt eq) [r] rs]


> insert_with_nuke lt eq x []          = [x]
> insert_with_nuke lt eq x r@(y:ys)
>        | x `lt` y     = x:r
>	 | x `eq` y    = r
>        | otherwise = y:insert_with_nuke lt eq x ys

> merge_with_nuke _ _ []     ys      = ys
> merge_with_nuke _ _ xs     []      = xs
> merge_with_nuke lt eq (x:xs) (y:ys)
>        | x `lt` y  = x : merge_with_nuke lt eq xs (y:ys)
>	 | x `eq` y  = x : merge_with_nuke lt eq xs ys
>        | otherwise = y : merge_with_nuke lt eq (x:xs) ys

> union :: (Ord a) => Set a -> Set a -> Set a
> union a b = merge_with_nuke (<) (==) a b

> merge_with_nuke_Int []     ys      = ys
> merge_with_nuke_Int xs     []      = xs
> merge_with_nuke_Int (x:xs) (y:ys)
>        | x < y     = x : merge_with_nuke_Int xs (y:ys)
>	 | x == y    = x : merge_with_nuke_Int xs ys
>        | otherwise = y : merge_with_nuke_Int (x:xs) ys

- tmp: poor man's specialisation

> union_Int :: Set Int -> Set Int -> Set Int
> union_Int a b = merge_with_nuke_Int a b

-- and, union of a whole list of sets:

> unionManySets :: (Ord a) => [Set a] -> Set a
> unionManySets = foldb union

> elementOf :: (Ord a) => a -> Set a -> Bool
> elementOf x y = elementOf_l (==) (>) x y

 elementOf_l :: (Ord a) => a -> [a] -> Bool

> elementOf_l eq gt x []     = False
> elementOf_l eq gt x (y:ys) 
>	= (x `eq` y) || (x `gt` y && elementOf_l eq gt x ys)

> isEmptySet :: (Ord a) => Set a -> Bool
> isEmptySet []    = True
> isEmptySet other = False

> sizeOfSet :: Set a -> Int
> sizeOfSet a = length a

> mapSet :: (Ord a,Ord b) => (a -> b) -> Set a -> Set b
> mapSet = map

> concatSet :: (Ord a) => Set (Set a) -> Set a
> concatSet = mkSet . concat

> concatMapSet :: (Ord b) => (a -> Set b) -> Set a -> Set b
> concatMapSet f [] = emptySet
> concatMapSet f a  = foldb union (map f a)

> filterSet :: (Ord a) => (a -> Bool) -> Set a -> Set a
> filterSet = filter

> subtractSet :: (Ord a) => Set a -> Set a -> Set a
> subtractSet a b = subtract_l (<) (==) a b

> subtract_l _ _ x [] = x
> subtract_l _ _ [] _ = []
> subtract_l lt eq x@(a:as) y@(b:bs) 
>	| a `lt` b = a : subtract_l lt eq as y
>	| a `eq` b = subtract_l lt eq as bs
>	| otherwise = subtract_l lt eq x bs



-----

> {-
> closure1 :: GrammarInfo -> ([Name] -> Set Name) -> Set Lr1Item -> [Lr1Item]
> closure1 gram first set
>       = setToList (fst (mkClosure (\(_,new) _ -> isEmptySet new) 
>				    addItems 
>			 	    (emptySet,set)))
>	where

>	addItems :: (Set Lr1Item,Set Lr1Item) -> (Set Lr1Item,Set Lr1Item)
>	addItems (oldItems,newItems) = (newOldItems, newNewItems)
> 	  where
>		newOldItems = newItems `union` oldItems
>		newNewItems = subtractSet (concatMapSet fn newItems)
>					   newOldItems

				(concat (map fn newItems)))

>		fn :: Lr1Item -> Set Lr1Item
>       	fn (rule,dot,a) = 
>	   	  case lookupProdNo gram rule of
>		   (name,lhs,_) -> 
>		      case drop dot lhs of
>			(b@(NonTerminal nt):beta) ->
>				let terms = setToList (first (beta ++ [a]))
>				    bRules = lookupProdsOfName gram b in
>					mkSet [ (rule,0,b) | rule <- bRules,
>						       b <- terms ]
>			_ -> emptySet
> -}
