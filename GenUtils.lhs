Some General Utilities, including sorts, etc.
This is realy just an extended prelude.
All the code below is understood to be in the public domain.

> module GenUtils (

>       ourLet, thd, partition', tack, 
>       assocMaybe, assocMaybeErr,
>       arrElem,
>       arrCond,
>       memoise,
>       Maybe(..),
>	thenMaybe,returnMaybe,handleMaybe, findJust,
>       MaybeErr(..),
>       mapMaybe,
>       maybeToBool,
>       maybeToObj,
>       maybeMap,
>       joinMaybe,
>       mkClosure,
>       foldb,
>       sortWith,
>       sort,
>       cjustify,
>       ljustify,
>       rjustify,
>       space,
>       copy,
>	combinePairs,
>	trace		-- re-export it 
>        ) where

#ifndef __GLASGOW_HASKELL__

> import 
> 	Maybe (Maybe(..))	-- on a different line to confuse mkdependHS (!)
> import 
> 	Trace

#else

#endif

> infixr 3 `ourLet`

%------------------------------------------------------------------------------

Here are two defs that everyone seems to define ... 
HBC has it in one of its builtin modules

#if defined(__GLASGOW_HASKELL__) || defined(__GOFER__)

> data Maybe a = Nothing | Just a deriving (Eq,Ord,Text)

#endif

> mapMaybe :: (a -> Maybe b) -> [a] -> [b]
> mapMaybe f [] = []
> mapMaybe f (a:r) = case f a of
>                       Nothing -> mapMaybe f r
>                       Just b  -> b : mapMaybe f r

#ifdef __GOFER__

> primitive primGenericEq "primGenericEq",
>           primGenericNe "primGenericNe",
>           primGenericLe "primGenericLe",
>           primGenericLt "primGenericLt",
>           primGenericGe "primGenericGe",
>           primGenericGt "primGenericGt"   :: a -> a -> Bool

> instance Text (Maybe a) where { showsPrec = primPrint } 
> instance Eq (Maybe a) where
>       (==) = primGenericEq 
>       (/=) = primGenericNe

> instance (Ord a) => Ord (Maybe a)
>   where 
>       Nothing  <=  _       = True
>       _        <=  Nothing = True
>       (Just a) <= (Just b) = a <= b

#endif

> maybeToBool :: Maybe a -> Bool
> maybeToBool (Just _) = True
> maybeToBool _        = False

> maybeToObj  :: Maybe a -> a
> maybeToObj (Just a) = a
> maybeToObj _        = error "Trying to extract object from a Nothing"

> maybeMap :: (a -> b) -> Maybe a -> Maybe b
> maybeMap f (Just a) = Just (f a)
> maybeMap f Nothing  = Nothing

> joinMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a 
> joinMaybe _ Nothing  Nothing  = Nothing
> joinMaybe _ (Just g) Nothing  = Just g
> joinMaybe _ Nothing  (Just g) = Just g
> joinMaybe f (Just g) (Just h) = Just (f g h)

> data MaybeErr a err = Succeeded a | Failed err deriving (Eq,Text)

#ifdef __GOFER__

> instance Text (MaybeErr a b) where { showsPrec = primPrint } 
> instance Eq (MaybeErr a b) where { (==) = primGenericEq } 

#endif

@mkClosure@ makes a closure, when given a comparison and iteration loop. 
Be careful, because if the functional always makes the object different, 
This will never terminate.

> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = match . iterate f
>   where
>       match (a:b:c) | a `eq` b = a
>       match (_:c)              = match c

> foldb :: (a -> a -> a) -> [a] -> a
> foldb f [] = error "can't reduce an empty list using foldb"
> foldb f [x] = x
> foldb f l  = foldb f (foldb' l)
>    where 
>       foldb' (x:y:x':y':xs) = f (f x y) (f x' y') : foldb' xs
>       foldb' (x:y:xs) = f x y : foldb' xs
>       foldb' xs = xs

Merge two ordered lists into one ordered list. 

> mergeWith               :: (a -> a -> Bool) -> [a] -> [a] -> [a] 
> mergeWith _ []     ys      = ys
> mergeWith _ xs     []      = xs
> mergeWith le (x:xs) (y:ys)
>        | x `le` y  = x : mergeWith le xs (y:ys)
>        | otherwise = y : mergeWith le (x:xs) ys

> insertWith              :: (a -> a -> Bool) -> a -> [a] -> [a]
> insertWith _ x []          = [x]
> insertWith le x (y:ys)
>        | x `le` y     = x:y:ys
>        | otherwise    = y:insertWith le x ys

Sorting is something almost every program needs, and this is the
quickest sorting function I know of.

> sortWith :: (a -> a -> Bool) -> [a] -> [a]
> sortWith le [] = []
> sortWith le lst = foldb (mergeWith le) (splitList lst)
>   where
>       splitList (a1:a2:a3:a4:a5:xs) = 
>                insertWith le a1 
>               (insertWith le a2 
>               (insertWith le a3
>               (insertWith le a4 [a5]))) : splitList xs
>       splitList [] = []
>       splitList (r:rs) = [foldr (insertWith le) [r] rs]

> sort :: (Ord a) => [a] -> [a]
> sort = sortWith (<=)

> thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
> thenMaybe m k = case m of
>                Just a -> k a
>                Nothing -> Nothing

> returnMaybe :: a -> Maybe a
> returnMaybe = Just

> handleMaybe :: Maybe a -> Maybe a -> Maybe a
> handleMaybe m k = case m of
>                Nothing -> k
>                _ -> m
 
> findJust :: (a -> Maybe b) -> [a] -> Maybe b
> findJust f = foldr handleMaybe Nothing . map f


Gofer-like stuff:

> cjustify, ljustify, rjustify :: Int -> String -> String
> cjustify n s = space halfm ++ s ++ space (m - halfm)
>                where m     = n - length s
>                      halfm = m `div` 2
> ljustify n s = s ++ space (max 0 (n - length s))
> rjustify n s = space (n - length s) ++ s

> space       :: Int -> String
> space n      = copy n ' '

> copy  :: Int -> a -> [a]      -- make list of n copies of x
> copy n x = take n xs where xs = x:xs

> ourLet k m = m k

> thd (a,b,c) = c
> 
> partition' :: (Eq b) => (a -> b) -> [a] -> [[a]]
> partition' f [] = []
> partition' f [x] = [[x]]
> partition' f (x:x':xs) | f x == f x' 
>    = tack x (partition' f (x':xs))
>                       | otherwise 
>    = [x] : partition' f (x':xs)

> tack x xss = (x : head xss) : tail xss

> combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
> combinePairs xs = 
>	combine [ (a,[b]) | (a,b) <- sortWith (\ (a,_) (b,_) -> a <= b) xs]
>  where
>	combine [] = []
>	combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
>	combine (a:r) = a : combine r
> 

> 
> assocMaybe :: (Eq a) => [(a,b)] -> a -> Maybe b
> assocMaybe env k = case [ val | (key,val) <- env, k == key] of
>                [] -> Nothing
>                (val:vs) -> Just val
> 
> assocMaybeErr :: (Eq a) => [(a,b)] -> a -> MaybeErr b String
> assocMaybeErr env k = case [ val | (key,val) <- env, k == key] of
>                        [] -> Failed "assoc: "
>                        (val:vs) -> Succeeded val
> 

> deSucc (Succeeded e) = e

Now some utilties involving arrays.
Here is a version of @elem@ that uses partual application
to optimise lookup.

> arrElem :: (Ix a) => [a] -> a -> Bool
> arrElem obj = \x -> inRange size x && arr ! x 
>   where
>       obj' = sort obj
>       size = (head obj',last obj')
>       arr = listArray size [ i `elem` obj | i <- range size ]

Here is the functional version of a multi-way conditional,
again using arrays, of course. Remember @b@ can be a function !
Note again the use of partiual application.

> arrCond :: (Ix a) 
>         => (a,a)                      -- the bounds
>         -> [(Assoc [a] b)]            -- the simple lookups
>         -> [(Assoc (a -> Bool) b)]    -- the functional lookups
>         -> b                          -- the default
>         -> a -> b                     -- the (functional) result

> arrCond bds pairs fnPairs def = (!) arr'
>   where
>       arr' = array bds [ t := head
>                       ([ r | (p := r) <- pairs, elem t p ] ++
>                        [ r | (f := r) <- fnPairs, f t ] ++
>                        [ def ])
>               | t <- range bds ]


You can use this function to simulate memoisation. For example:

      > fib = memoise (0,100) fib'
      >   where
      >       fib' 0 = 0
      >       fib' 1 = 0
      >       fib' n = fib (n-1) + fib (n-2)

will give a very efficent variation of the fib function.


> memoise :: (Ix a) => (a,a) -> (a -> b) -> a -> b
> memoise bds f = (!) arr
>   where arr = array bds [ t := f t | t <- range bds ]

> mapAccumR :: (acc -> x -> (acc, y))         -- Function of elt of input list
>                                     -- and accumulator, returning new
>                                     -- accumulator and elt of result list
>         -> acc                      -- Initial accumulator
>         -> [x]                      -- Input list
>         -> (acc, [y])               -- Final accumulator and result list
>
> mapAccumR f b []     = (b, [])
> mapAccumR f b (x:xs) = (b'', x':xs') where
>                                       (b'', x') = f b' x
>                                       (b', xs') = mapAccumR f b xs

> mapAccumL :: (acc -> x -> (acc, y)) 	-- Function of elt of input list
>					-- and accumulator, returning new
>					-- accumulator and elt of result list
>	    -> acc 			-- Initial accumulator
>	    -> [x] 			-- Input list
>	    -> (acc, [y])		-- Final accumulator and result list
>
> mapAccumL f b []     = (b, [])
> mapAccumL f b (x:xs) = (b'', x':xs') where
>					  (b', x') = f b x
>					  (b'', xs') = mapAccumL f b' xs

Here is the bi-directional version ...

> mapAccumB :: (accl -> accr -> x -> (accl, accr,y))
>					-- Function of elt of input list
>					-- and accumulator, returning new
>					-- accumulator and elt of result list
>	    -> accl 			-- Initial accumulator from left
>	    -> accr 			-- Initial accumulator from right
>	    -> [x] 			-- Input list
>	    -> (accl, accr, [y])	-- Final accumulator and result list
>
> mapAccumB f a b []     = (a,b,[])
> mapAccumB f a b (x:xs) = (a'',b'',y:ys)
>    where
>	(a',b'',y)    = f a b' x
>	(a'',b',ys) = mapAccumB f a' b xs


