%-----------------------------------------------------------------------------
This code is derived from the Set Module in the Glasgow Haskell Compiler, 
which in turn was derived from the utilities section of Simon PJ's second book.
It is therefore *not* covered under the GNU License.
%-----------------------------------------------------------------------------

> module Set (
>        Set,            -- abstract type
>        mkSet, setToList, emptySet, singletonSet,
>        union, unionManySets,
>        elementOf, isEmptySet,
>        sizeOfSet, mapSet, concatSet, concatMapSet, filterSet
>    ) where

> data Set a = MkSet [a] deriving (Eq,Ord)

> instance (Text a) => Text (Set a) where 
>    showsPrec _ (MkSet [])     = showString "{}"
>    showsPrec _ (MkSet (x:xs)) = showChar '{' . shows x . showl xs
>                      where showl []     = showChar '}'
>                            showl (x:xs) = showChar ',' . shows x . showl xs

#ifdef GOFER

> instance Eq (Set a) where { (==) = primGenericEq } 
> instance Ord [a] => Ord (Set a) where 
>       (MkSet a) <= (MkSet b) = a <= b

#endif

This is where we order the list and remove duplicates.

> emptySet :: Ord a => Set a
> emptySet = MkSet []

> singletonSet :: Ord a => a -> Set a
> singletonSet x = MkSet [x]

> setToList :: Ord a => Set a -> [a]
> setToList (MkSet xs) = xs

> mkSet :: (Ord a) => [a]  -> Set a
> mkSet xs = MkSet (sort_and_nuke_dups (==) (<) xs)
> --mkSet xs = MkSet (nub xs)

-- slightly tweaked for performance...

> sort_and_nuke_dups :: (a->a->Bool) -> (a->a->Bool) -> [a] -> [a]
> sort_and_nuke_dups eq lt []     = []
> sort_and_nuke_dups eq lt [x]    = [x]
> sort_and_nuke_dups eq lt (x:xs) = split x [] [] xs
>   where
>      split x lo hi [] = sort_and_nuke_dups eq lt lo ++ (x : sort_and_nuke_dups eq lt hi)
>      split x lo hi (y:ys) | y `lt` x = split x (y:lo) hi     ys
>                           | y `eq` x = split x lo     hi     ys
>                           | True     = split x lo     (y:hi) ys

> union :: (Ord a) => Set a -> Set a -> Set a
> union (MkSet a) (MkSet b)       = MkSet (union_l a b)
> --union (MkSet a) (MkSet b)       = mkSet (a ++ b)

> union_l :: (Ord a) => [a] -> [a] -> [a]
> union_l []        []                  = []
> union_l []        y                   = y
> union_l x         []                  = x
> union_l x@(a:as)  y@(b:bs)| a < b     = a : union_l as y
>                           | a == b    = a : union_l as bs
>                           | otherwise = b : union_l x  bs

-- and, union of a whole list of sets:

> unionManySets :: (Ord a) => [Set a] -> Set a
> unionManySets = foldr union emptySet

> elementOf :: (Ord a) => a -> Set a -> Bool
> elementOf x (MkSet y)   = elementOf_l x y

> elementOf_l :: (Ord a) => a -> [a] -> Bool
> elementOf_l x []     = False
> elementOf_l x (y:ys) = (x == y) || (x > y && elementOf_l x ys)

> isEmptySet :: (Ord a) => Set a -> Bool
> isEmptySet (MkSet [])   = True
> isEmptySet other        = False

> sizeOfSet :: Set a -> Int
> sizeOfSet (MkSet a) = length a

> mapSet :: (Ord a,Ord b) => (a -> b) -> Set a -> Set b
> mapSet f (MkSet a) = (mkSet (map f a))

> concatSet :: (Ord a) => Set (Set a) -> Set a
> concatSet (MkSet a) = mkSet (concat [ x | (MkSet x) <- a ])

> concatMapSet :: (Ord b) => (a -> Set b) -> Set a -> Set b
> concatMapSet f (MkSet a) = mkSet (concat (map (setToList.f) a))

> filterSet :: (Ord a) => (a -> Bool) -> Set a -> Set a
> filterSet f (MkSet a) = (mkSet (filter f a))
