-----------------------------------------------------------------------------
Some General Utilities, including sorts, etc.
This is realy just an extended prelude.
All the code below is understood to be in the public domain.
-----------------------------------------------------------------------------

> module GenUtils (

>       MaybeErr(..),
>       mkClosure,
>       foldb,
>       listArray',
>       ljustify,
>       space,
>       combinePairs,
>       mapDollarDollar,
>       str, char, nl, brack, brack',
>       interleave, interleave',
>       strspace, maybestr
>        ) where

> import Data.Char  (isAlphaNum)
> import Data.List
> import Data.Array (Array, listArray)

%------------------------------------------------------------------------------

> data MaybeErr a err = Succeeded a | Failed err deriving (Eq,Show)

@mkClosure@ makes a closure, when given a comparison and iteration loop.
Be careful, because if the functional always makes the object different,
This will never terminate.

> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = match . iterate f
>   where
>       match (a:b:_) | a `eq` b = a
>       match (_:c)              = match c
>       match [] = error "Can't happen: match []"

> foldb :: (a -> a -> a) -> [a] -> a
> foldb _ [] = error "can't reduce an empty list using foldb"
> foldb _ [x] = x
> foldb f l  = foldb f (foldb' l)
>    where
>       foldb' (x:y:x':y':xs) = f (f x y) (f x' y') : foldb' xs
>       foldb' (x:y:xs) = f x y : foldb' xs
>       foldb' xs = xs


Gofer-like stuff:

> ljustify :: Int -> String -> String
> ljustify n s = s ++ space (max 0 (n - length s))

> space       :: Int -> String
> space n      = replicate n ' '

> combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
> combinePairs xs =
>       combine [ (a,[b]) | (a,b) <- sortBy (\ (a,_) (b,_) -> compare a b) xs]
>  where
>       combine [] = []
>       combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
>       combine (a:r) = a : combine r
>

> listArray' :: (Int,Int) -> [a] -> Array Int a
> listArray' (low,up) elems =
>       if length elems /= up-low+1 then error "wibble" else
>       listArray (low,up) elems



Replace $$ with an arbitrary string, being careful to avoid ".." and '.'.

> mapDollarDollar :: String -> Maybe (String -> String)
> mapDollarDollar code0 = go code0 ""
>   where go code acc =
>           case code of
>               [] -> Nothing
>
>               '"'  :r    -> case reads code :: [(String,String)] of
>                                []       -> go r ('"':acc)
>                                (s,r'):_ -> go r' (reverse (show s) ++ acc)
>               a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc)
>               '\'' :r    -> case reads code :: [(Char,String)] of
>                                []       -> go r ('\'':acc)
>                                (c,r'):_ -> go r' (reverse (show c) ++ acc)
>               '\\':'$':r -> go r ('$':acc)
>               '$':'$':r  -> Just (\repl -> reverse acc ++ repl ++ r)
>               c:r  -> go r (c:acc)


%-------------------------------------------------------------------------------
Fast string-building functions.

> str :: String -> String -> String
> str = showString
> char :: Char -> String -> String
> char c = (c :)
> interleave :: String -> [String -> String] -> String -> String
> interleave s = foldr (\a b -> a . str s . b) id
> interleave' :: String -> [String -> String] -> String -> String
> interleave' s = foldr1 (\a b -> a . str s . b)

> strspace :: String -> String
> strspace = char ' '
> nl :: String -> String
> nl = char '\n'

> maybestr :: Maybe String -> String -> String
> maybestr (Just s)     = str s
> maybestr _            = id

> brack :: String -> String -> String
> brack s = str ('(' : s) . char ')'
> brack' :: (String -> String) -> String -> String
> brack' s = char '(' . s . char ')'
