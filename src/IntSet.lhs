%
% (c) The GHC Team, 1998
%
\section[IntSet]{An implementation of fast integer sets using bitmaps}

IntSets are useful for storing sets of small integers.  Most
operations are O(n) where n is the largest element in the set, but the
overhead is small because the set is stored as a bitmap and (for
example) union is bitwise-or.

\begin{code}
module IntSet (
	IntSet,		-- abstract type
	mkIS,		-- :: [Int] -> IntSet
	listIS,		-- :: IntSet -> [Int]
	emptyIS,	-- :: IntSet
	isEmptyIS,	-- :: IntSet -> Bool
	elemIS,		-- :: Int -> IntSet -> Bool
	unitIS,		-- :: Int -> IntSet
	unionIS,	-- :: IntSet -> IntSet -> IntSet
	minusIS, 	-- :: IntSet -> IntSet -> IntSet
	intersectIS, 	-- :: IntSet -> IntSet -> IntSet
	intsIS		-- :: IntSet -> [Int]
    ) where

import Bits
import Word

-- other possible representations:
--	data IntSet = EmptyIS | IS Word# IntSet

-- use Word32 internally; probably should use natural word size for the
-- host architecture.

type WordRep = Word32
word_size = 32

newtype IntSet = IntSet [WordRep]

instance Eq IntSet where
	(IntSet is) == (IntSet js) = is == js

emptyIS :: IntSet
emptyIS = IntSet []

isEmptyIS :: IntSet -> Bool
isEmptyIS (IntSet []) = True
isEmptyIS (IntSet _ ) = False

unitIS :: Int -> IntSet
unitIS e = IntSet (addIS e [])

mkIS :: [Int] -> IntSet
mkIS ints = IntSet (foldr addIS [] ints)

intsIS  :: IntSet -> [Int]
intsIS (IntSet is) = map fromIntegral is

addIS :: Int -> [WordRep] -> [WordRep]
addIS i [] = addIS i [0]
addIS i (w:ws)
  | i < word_size = (w .|. (1 `shiftL` i)) : ws
  | otherwise     = w : addIS (i-word_size) ws

elemIS :: Int -> IntSet -> Bool
elemIS i is = not (isEmptyIS (is `intersectIS` unitIS i))

unionIS :: IntSet -> IntSet -> IntSet 
unionIS (IntSet is) (IntSet js)  = IntSet (go is js)
  where go [] is = is
	go is [] = is
	go (i:is) (j:js) = (i .|. j) : go is js

minusIS :: IntSet -> IntSet -> IntSet
minusIS (IntSet is) (IntSet js)  = IntSet (canonIS (go is js))
  where go [] js = []
	go is [] = is
	go (i:is) (j:js) = (i .&. complement j) : go is js

intersectIS :: IntSet -> IntSet -> IntSet
intersectIS (IntSet is) (IntSet js) = IntSet (canonIS (go is js))
  where go [] js = []
	go is [] = []
	go (i:is) (j:js) = (i .&. j) : go is js

canonIS :: [WordRep] -> [WordRep]
canonIS []  = []
canonIS (0:ws) = case canonIS ws of
			[]  -> []
			ws' -> 0:ws'
canonIS (w:ws) = w : canonIS ws

listIS  :: IntSet -> [Int]            
listIS (IntSet is) = listify_wds is 0
    where 
	listify_wds [] n = []
	listify_wds (w:ws) n = listify_one w n (listify_wds ws (n+word_size))

	listify_one 0 n r = r
	listify_one w n r = let rest = listify_one (w `shiftR` 1) (n + 1) r
			    in if (w .&. 1 == 0) 
				then rest 
				else n : rest
\end{code}
