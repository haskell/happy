This module demonstrates a Happy bug (in version <= 1.10).

> {
> module Main where
> import System.IO
> import Control.Exception as Exception
>
> {-# LANGUAGE NoImplicitPrelude #-}
> import qualified Prelude
> import Prelude ((>>))
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
> happyError s = Prelude.error (Prelude.concatMap Prelude.show s)
>
> data Tok = Plus | Minus | Num Prelude.Int deriving Prelude.Show
>
> data Syn = Plus' Syn Syn | Minus' Syn Syn | Num' Prelude.Int deriving Prelude.Show

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
>   Exception.try (Prelude.print test1 >> Prelude.fail "Test failed.") :: Prelude.IO (Prelude.Either ErrorCall ())
>   Exception.try (Prelude.print test2 >> Prelude.fail "Test failed.") :: Prelude.IO (Prelude.Either ErrorCall ())
>   Exception.try (Prelude.print test3 >> Prelude.fail "Test failed.") :: Prelude.IO (Prelude.Either ErrorCall ())

> }
