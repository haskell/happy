-----------------------------------------------------------------------------
Some General Utilities, including sorts, etc.
This is realy just an extended prelude.
All the code below is understood to be in the public domain.
-----------------------------------------------------------------------------

> module GenUtils (

>       mkClosure,
>       combinePairs,
>       mapDollarDollar,
>       str, char, nl, brack, brack',
>       interleave, interleave',
>       strspace, maybestr,
>       die, dieHappy
>        ) where

> import Data.Char  (isAlphaNum)
> import Data.Ord   (comparing)
> import Data.List  (sortBy)
> import Control.Monad
> import System.IO  (stderr, hPutStr)
> import System.Environment
> import System.Exit (exitWith, ExitCode(..))

%------------------------------------------------------------------------------

@mkClosure@ makes a closure, when given a comparison and iteration loop.
Be careful, because if the functional always makes the object different,
This will never terminate.

> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = match . iterate f
>   where
>       match (a:b:_) | a `eq` b = a
>       match (_:c)              = match c
>       match [] = error "Can't happen: match []"


Gofer-like stuff:

> combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
> combinePairs xs =
>       combine [ (a,[b]) | (a,b) <- sortBy (comparing fst) xs]
>  where
>       combine [] = []
>       combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
>       combine (a:r) = a : combine r
>


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

> die :: String -> IO a
> die s = hPutStr stderr s >> exitWith (ExitFailure 1)

> dieHappy :: String -> IO a
> dieHappy s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)

> getProgramName :: IO String
> getProgramName = liftM (`withoutSuffix` ".bin") getProgName
>   where str' `withoutSuffix` suff
>            | suff `isSuffixOf` str' = take (length str' - length suff) str'
>            | otherwise              = str'
