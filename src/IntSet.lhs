-----------------------------------------------------------------------------
$Id: IntSet.lhs,v 1.1 1997/02/11 13:12:07 simonm Exp $

Efficient implementation of small integer sets.

(c) 1996 Simon Marlow
-----------------------------------------------------------------------------

> module IntSet (IntSet(..), emptyIntSet, singletonIntSet, addToIntSet,
>		 unionIntSets, intSetToSetInt) where

> import Word

> data IntSet = [Word]

> emptyIntSet :: IntSet
> emptyIntSet = []

> singletonIntSet :: Int -> IntSet
> singletonIntSet i = addToIntSet i emptyIntSet

> addToIntSet :: Int -> IntSet -> IntSet
> addToIntSet j s
>	| m < ls = 
>		let (before, x:after) = splitAt m s in
>		(before, (x `bitOr` (i `bitLsh` n)) : after)
>	| otherwise = take m (s ++ repeat 0) : (i `bitShr` n)
>  where
>	i = 
>	m = i `div` 32
>	n = i `mod` 32
>	ls = length s

> unionIntSets :: IntSet -> IntSet -> IntSet
> unionIntSets [] [] = []
> unionIntSets [] (t:ts) = t : unionIntSets [] ts
> unionIntSets (s:ss) [] = s : unionIntSets ss []
> unionIntSets (s:ss) (t:ts) = t `bitOr` s : unionIntSets ss ts

-----------------------------------------------------------------------------

> masks = zip (map (1 `bitShl`) [0..31]) [0..31]

> intSetToSetInt :: IntSet -> [Int]
> intSetToSetInt s = toSetInt 0 s

> toSetInt :: Int -> IntSet -> [Int]
> toSetInt _ [] = []
> toSetInt i (s:ss) =
>	map (+i) [ j | (o,j) <- masks, o `bitAnd` s > 0 ]
>	++ toSetInt (i+32) ss
