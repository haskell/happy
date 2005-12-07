This module demonstrates a Happy bug (in version <= 1.10).

> {
> module Main where
> import IO
> import Control.Exception as Exception
> }
> 
> %name parse
> 
> %tokentype { Tok }
> %token
>       '+'  { Plus }
>       '-'  { Minus }
>       int  { Num $$ }
> 
> %nonassoc '+' '-'
> 
> %%

Ambiguos grammar.

> E    : E '+' E  { Plus' $1 $3 }
>      | E '-' E  { Minus' $1 $3 }
>      | int      { Num' $1 }

> {
> happyError :: [Tok] -> a
> happyError s = error (concatMap show s)
> 
> data Tok = Plus | Minus | Num Int deriving Show
> 
> data Syn = Plus' Syn Syn | Minus' Syn Syn | Num' Int deriving Show

All the examples below should fail. None of them does so
under Happy v1.8, and only the first one under Happy v1.9
and v1.10.

> test1 = parse tokens1
> test2 = parse tokens2
> test3 = parse tokens3
> 
> tokens1 = [Num 6, Plus, Num 7, Plus, Num 8]
> tokens2 = [Num 6, Plus, Num 7, Minus, Num 8]
> tokens3 = [Num 6, Minus, Num 7, Minus, Num 8]

The generated info files seem correct, so there is probably
something wrong with the table generation.

These errors only show up when one uses Happy with the -a
flag (and only that flag). I know that it's no point in
using just that flag, but I happened to be doing so while
trying the code out with Hugs. (Hugs didn't like the code
generated with GHC extensions, -gac.)

> main = do
>   Exception.tryJust errorCalls (print test1 >> fail "Test failed.") 
>   Exception.tryJust errorCalls (print test2 >> fail "Test failed.") 
>   Exception.tryJust errorCalls (print test3 >> fail "Test failed.")

> }
