-----------------------------------------------------------------------------
Some General Utilities, including sorts, etc.
This is realy just an extended prelude.
All the code below is understood to be in the public domain.
-----------------------------------------------------------------------------

> module GenUtils (
>       str, char, nl, brack, brack',
>       interleave, interleave',
>       strspace, maybestr,
>       die, dieHappy,
>       optPrint,
>       getProgramName
>        ) where

> import Data.List  (isSuffixOf)
> import Control.Monad
> import System.IO  (stderr, hPutStr)
> import System.Environment
> import System.Exit (exitWith, ExitCode(..))

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

> optPrint :: Bool -> IO () -> IO ()
> optPrint b io =
>       when b (putStr "\n---------------------\n" >> io)
