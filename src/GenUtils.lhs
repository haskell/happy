-----------------------------------------------------------------------------
Some General Utilities, including sorts, etc.
This is realy just an extended prelude.
All the code below is understood to be in the public domain.
-----------------------------------------------------------------------------

> module GenUtils (

>       combinePairs,
>       str, char, nl, brack, brack',
>       interleave, interleave',
>       strspace, maybestr
>        ) where

> import Data.Char  (isAlphaNum)
> import Data.Ord   (comparing)
> import Data.List  (sortBy)

%------------------------------------------------------------------------------

Gofer-like stuff:

> combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
> combinePairs xs =
>       combine [ (a,[b]) | (a,b) <- sortBy (comparing fst) xs]
>  where
>       combine [] = []
>       combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
>       combine (a:r) = a : combine r
>


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
