-----------------------------------------------------------------------------
A set ADT.

This code is derived from the Set Module in the Glasgow Haskell
Compiler, which in turn was derived from the utilities section of
Simon PJ's second book.  It is therefore *not* covered under the GNU
License.
-----------------------------------------------------------------------------

> module Set (
>        Set, member,
>        fromList, toAscList, empty, singleton,
>        union,
>        null,
>        filter, fold,
>	 difference
>    ) where

> import Prelude hiding ( null, filter )
> import GenUtils

> newtype Set a = Set [a] deriving Eq

This is where we order the list and remove duplicates.

> member :: Ord a => a -> Set a -> Bool
> member x (Set xs) = x `elem` xs

> empty :: Ord a => Set a
> empty = Set []

> singleton :: Ord a => a -> Set a
> singleton x = Set [x]

> toAscList :: Ord a => Set a -> [a]
> toAscList (Set xs) = xs

> fromList :: (Ord a) => [a] -> Set a
> fromList xs = Set (sort_and_nuke_dups (<) (==) xs)

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
> union (Set a) (Set b) = Set (merge_with_nuke (<) (==) a b)

> null :: (Ord a) => Set a -> Bool
> null (Set [])    = True
> null (Set other) = False

> filter :: (Ord a) => (a -> Bool) -> Set a -> Set a
> filter p (Set xs) = Set [ x | x <- xs, p x ]

> fold :: (a -> b -> b) -> b -> Set a -> b
> fold f z (Set xs) = foldr f z xs

> difference :: (Ord a) => Set a -> Set a -> Set a
> difference (Set a) (Set b) = Set (subtract_l (<) (==) a b)

> subtract_l _ _ x [] = x
> subtract_l _ _ [] _ = []
> subtract_l lt eq x@(a:as) y@(b:bs) 
>	| a `lt` b = a : subtract_l lt eq as y
>	| a `eq` b = subtract_l lt eq as bs
>	| otherwise = subtract_l lt eq x bs
